package FFT

import chisel3._
import chisel3.experimental._
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.util._
import craft._
import dspblocks._
import dspjunctions._
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import scala.math._


class DirectFFTIO[T <: Data : Real](genMid: DspComplex[T], genOut: DspComplex[T], lanes: Int) extends Bundle {
  val in = Input(ValidWithSync(Vec(lanes, genMid)))
  val out = Output(ValidWithSync(Vec(lanes, genOut)))
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * direct form version
  * note, this is always a p-point FFT, though the twiddle factors will be different if p < n
  *
  * @tparam T
  */
class DirectFFT[T <: Data : Real](config: FFTConfig[T], genMid: DspComplex[T], genTwiddle: DspComplex[T], genOutFull: DspComplex[T]) extends Module {
  val io = IO(new DirectFFTIO[T](genMid, genOutFull, config.lanes))

  // synchronize
  val valid_delay = RegNext(io.in.valid)
  val sync = Wire(UInt(log2Ceil(config.bp).W))
  sync := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  io.out.sync := ShiftRegisterWithReset(io.in.valid && sync === (config.bp - 1).U, config.direct_pipe, 0.U) // should valid keep sync from propagating?
  io.out.valid := ShiftRegisterWithReset(io.in.valid, config.direct_pipe, 0.U)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag
  // This should work and would simplify the firrtl, but... it doesn't seem to work

  val twiddle_rom = config.dtwiddles.map(i => {
    VecInit(i.map(j => {
      val real = Wire(genTwiddleReal.cloneType)
      val imag = Wire(genTwiddleImag.cloneType)
      real := Real[T].fromDouble(j(0), genTwiddleReal)
      imag := Real[T].fromDouble(j(1), genTwiddleImag)
      val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
      twiddle.real := real
      twiddle.imag := imag
      twiddle
    }))
  })


  val stage_outputs = List.fill(log2Ceil(config.lanes) + 1)(List.fill(config.lanes)(Wire(genOutFull)))
  io.in.bits.zip(stage_outputs(0)).foreach { case (in, out) => out := in }

  // create the FFT hardware
  for (i <- 0 until log2Ceil(config.lanes)) {
    for (j <- 0 until config.lanes / 2) {

      var skip = pow(2, log2Ceil(config.n / 2) - (i + log2Ceil(config.bp))).toInt
      skip = if (config.unscrambleIn == false) skip else bit_reverse(skip, log2Ceil(config.lanes))
      val j_rev = j
      val i_rev =if (config.unscrambleIn == false) i else log2Ceil(config.lanes) - i- 1
      val start = ((j_rev % skip) + floor(j_rev / skip) * skip * 2).toInt
      // hook it up
      val outputs = List(stage_outputs(i + 1)(start), stage_outputs(i + 1)(start + skip))
      if (config.unscrambleIn == false) {
        val butterfly_outputs = Butterfly[T](Seq(stage_outputs(i)(start), stage_outputs(i)(start + skip)), twiddle_rom(config.tdindices(j)(i_rev))(sync))
        outputs.zip(butterfly_outputs).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i + log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram") }
      } else {
        val butterfly_outputs = ButterflyDIF[T](Seq(stage_outputs(i)(start), stage_outputs(i)(start + skip)), twiddle_rom(config.tdindices(j)(i_rev))(sync))
        outputs.zip(butterfly_outputs).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i + log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram") }
      }
      // TODO: pipeline reorder
    }
  }

  // wire up top-level outputs
  // note, truncation happens here!

  if (config.unscrambleOut == true) {
    io.out.bits := unscramble(stage_outputs(log2Ceil(config.lanes)), config.lanes)
  } else {
    io.out.bits := stage_outputs(log2Ceil(config.lanes))
  }
}

class BiplexFFTIO[T <: Data : Real](lanes: Int, genIn: DspComplex[T], genMid: DspComplex[T]) extends Bundle {
  val in = Input(ValidWithSync(Vec(lanes, genIn)))
  val out = Output(ValidWithSync(Vec(lanes, genMid)))
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * biplex pipelined version
  * note, this is always a bp-point FFT
  *
  * @tparam T
  */
class BiplexFFT[T <: Data : Real](config: FFTConfig[T], genIn: DspComplex[T], genMid: DspComplex[T], genTwiddle: DspComplex[T]) extends Module {
  val io = IO(new BiplexFFTIO[T](config.lanes, genIn, genMid))

  // synchronize
  var stage_delays = (0 until log2Ceil(config.bp) + 1).map(x => {
    if (x == log2Ceil(config.bp)) config.bp / 2 else (config.bp / pow(2, x + 1)).toInt
  })

  stage_delays = if (config.unscrambleIn==false) stage_delays else stage_delays.reverse

  val sync = List.fill(log2Ceil(config.bp) + 1)(Wire(UInt(width = log2Ceil(config.bp).W)))
  val valid_delay = RegNext(io.in.valid)
  sync(0) := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  sync.drop(1).zip(sync).zip(stage_delays).foreach { case ((next, prev), delay) => next := ShiftRegisterWithReset(prev, delay, 0.U) }
  io.out.sync := (sync(log2Ceil(config.bp)) === ((config.bp / 2 - 1 + config.biplex_pipe) % config.bp).U) && (sync(log2Ceil(config.bp)) =/= 0.U)
  io.out.valid := ShiftRegisterWithReset(io.in.valid, stage_delays.reduce(_ + _) + config.biplex_pipe, 0.U)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag

//  println("[--DEBUG--]Stage_delays are:", stage_delays)

  val twiddle_rom = config.btwiddles.map(i => {
    VecInit(i.map(j => {
      val real = Wire(genTwiddleReal.cloneType)
      val imag = Wire(genTwiddleImag.cloneType)
      real := Real[T].fromDouble(j(0), genTwiddleReal)
      imag := Real[T].fromDouble(j(1), genTwiddleImag)
      val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
      twiddle.real := real
      twiddle.imag := imag
      twiddle
    }))
  })
  // wire up top-level outputs
  val stage_outputs = List.fill(log2Ceil(config.bp) + 2)(List.fill(config.lanes)(Wire(genIn)))
  io.in.bits.zip(stage_outputs(0)).foreach { case (in, out) => out := in }


  // create the FFT hardware
  for (i <- 0 until log2Ceil(config.bp) + 1) {
    for (j <- 0 until config.lanes / 2) {

      val skip = 1
      val start = j * 2
      // hook it up
      // last stage just has one extra permutation, no butterfly

      if (config.unscrambleIn == false) {
        val mux_out = BarrelShifter(VecInit(stage_outputs(i)(start),
          ShiftRegisterMem(stage_outputs(i)(start + skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0_sram")),
          ShiftRegisterMem(sync(i)(log2Ceil(config.bp) - 1 - {if (i == log2Ceil(config.bp)) 0 else i}),
          {if (i == 0) 0 else config.pipe.dropRight(log2Ceil(config.n) - i).reduceRight(_ + _)},
            name = this.name + s"_${i}_${j}_mux1_sram"))
        if (i == log2Ceil(config.bp)) {
          Seq(stage_outputs(i + 1)(start), stage_outputs(i + 1)(start + skip)).zip(
            Seq(
              ShiftRegisterMem(
                  mux_out(0),
                  stage_delays(i),
                  name = this.name + s"_${i}_${j}_last_sram"
                ),
              mux_out(1)
            )
          ).foreach { x => x._1 := x._2 }
        } else {
          Seq(stage_outputs(i + 1)(start), stage_outputs(i + 1)(start + skip)).zip(
            Butterfly(
              Seq(
                ShiftRegisterMem(
                  mux_out(0),
                  stage_delays(i),
                  name = this.name + s"_${i}_${j}_pipeline0_sram"
                ),
                mux_out(1)
              ),
              twiddle_rom(i)(sync(i + 1))
            )
          ).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1_sram") }
        }
      } else {
        val mux_out = BarrelShifter(VecInit(stage_outputs(i)(start),
          ShiftRegisterMem(stage_outputs(i)(start + skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0_sram")),
          ShiftRegisterMem(sync(i)({if (i == 0) log2Ceil(config.bp)-1 else i-1}),
            {if (i == 0) 0 else config.pipe.dropRight(log2Ceil(config.n) - i).reduceRight(_ + _)}, name = this.name + s"_${i}_${j}_mux1_sram"))
        //TODO: why 0 is special here?

        if (i == log2Ceil(config.bp)) {
          Seq(stage_outputs(i + 1)(start), stage_outputs(i + 1)(start + skip)).zip(
            Seq(
              ShiftRegisterMem(
                mux_out(0),
                stage_delays(i),
                name = this.name + s"_${i}_${j}_last_sram"
              ),
              mux_out(1)
            )
          ).foreach { x => x._1 := x._2 }
        } else {
          Seq(stage_outputs(i + 1)(start), stage_outputs(i + 1)(start + skip)).zip(
            ButterflyDIF(
              Seq(
                ShiftRegisterMem(
                  mux_out(0),
                  stage_delays(i),
                  name = this.name + s"_${i}_${j}_pipeline0_sram"
                ),
                mux_out(1)
              ),
              twiddle_rom(log2Ceil(config.bp)-1-i)(sync(i+1))
            )
          ).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1_sram") }
        }
      }

    }
  }
  // wire up top-level output
  io.out.bits := stage_outputs(log2Ceil(config.bp) + 1)

}

//TODO: Biplex's width should expand for unscrambleIn config
/**
  * IO Bundle for FFT
  *
  * @tparam T
  */
class FFTIO[T <: Data : Real](lanes: Int, genIn: DspComplex[T], genOut: DspComplex[T]) extends Bundle {

  val in = Input(ValidWithSync(Vec(lanes, genIn)))
  val out = Output(ValidWithSync(Vec(lanes, genOut)))

  val data_set_end_status = Output(Bool())
  val data_set_end_clear = Input(Bool())
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * mixed version
  * note, this is always an n-point FFT
  *
  * @tparam T
  */
class FFT[T <: Data : Real](val config: FFTConfig[T]) extends Module {

  require(config.lanes >= 2, "Must have at least 2 parallel inputs")
  require(isPow2(config.lanes), "FFT parallelism must be a power of 2")
  require(config.lanes <= config.n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")

  val io = IO(new FFTIO(config.lanes, config.genIn, config.genOut))

  // calculate direct FFT input bitwidth
  // this is just the input total width + growth of 1 bit per biplex stage
  val genMid: DspComplex[T] = {
    if (config.bp == 1) {
      config.genIn
    }
    else {
      val growth = if(config.unscrambleIn==false )log2Ceil(config.n) else log2Ceil(config.n)
      config.genIn.underlyingType() match {
        case "fixed" =>
          config.genIn.real.asInstanceOf[FixedPoint].binaryPoint match {
            case KnownBinaryPoint(binaryPoint) =>
              val totalBits = config.genIn.real.getWidth + growth
              DspComplex(FixedPoint(totalBits.W, binaryPoint.BP), FixedPoint(totalBits.W, binaryPoint.BP)).asInstanceOf[DspComplex[T]]
            case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
          }
        case "sint" => {
          val totalBits = config.genIn.real.getWidth + growth
          DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
        }
        case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
      }
    }
  }

  // calculate twiddle factor bitwidth
  // total input bits
  val genTwiddleBiplex: DspComplex[T] = {
//    val growth = log2Ceil(config.bp)
    //TODO: figure out the right way to calculate width
    val growth = if(config.unscrambleIn==false )log2Ceil(config.n) else log2Ceil(config.n)
    config.genIn.underlyingType() match {
      case "fixed" =>
        config.genIn.asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
            DspComplex(FixedPoint(totalBits.W, (totalBits - 2).BP), FixedPoint(totalBits.W, (totalBits - 2).BP)).asInstanceOf[DspComplex[T]]
          case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
        }
      case "sint" => {
        val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
        DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
      }
      case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
    }
  }

  val genTwiddleDirect: DspComplex[T] = {
    val growth = if(config.unscrambleIn==false )log2Ceil(config.n) else log2Ceil(config.n)
    config.genIn.underlyingType() match {
      case "fixed" =>
        config.genIn.asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
            DspComplex(FixedPoint(totalBits.W, (totalBits - 2).BP), FixedPoint(totalBits.W, (totalBits - 2).BP)).asInstanceOf[DspComplex[T]]
          case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
        }
      case "sint" => {
        val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
        DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
      }
      case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
    }
  }

  // calculate direct FFT output bitwidth
  // this is just the input total width + growth of 1 bit per FFT stage
  val genOutDirect: DspComplex[T] = {
    if (config.bp == 1) {
      config.genIn
    }
    else {
//      val growth = log2Ceil(config.n)
      val growth = if(config.unscrambleIn==false )log2Ceil(config.n) else log2Ceil(config.n)
      config.genIn.underlyingType() match {
        case "fixed" =>
          config.genIn.asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
            case KnownBinaryPoint(binaryPoint) =>
              val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
              DspComplex(FixedPoint(totalBits.W, binaryPoint.BP), FixedPoint(totalBits.W, binaryPoint.BP)).asInstanceOf[DspComplex[T]]
            case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
          }
        case "sint" => {
          val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
          DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
        }
        case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
      }
    }
  }

  // feed in zeros when invalid
  val in = Wire(ValidWithSync(Vec(config.lanes, config.genIn)))
  when(io.in.valid) {
    in.bits := io.in.bits
  }.otherwise {
    in.bits.foreach { case b =>
      b.real := Real[T].zero
      b.imag := Real[T].zero
    }
  }
  in.valid := io.in.valid
  in.sync := io.in.sync

  // data set end flag
  val valid_delay = RegNext(io.out.valid)
  val dses = RegInit(false.B)
  when(io.data_set_end_clear) {
    dses := false.B
  }.elsewhen(valid_delay & ~io.out.valid) {
    dses := true.B
  }
  io.data_set_end_status := dses

  // instantiate sub-FFTs
  if (config.unscrambleIn == false) {
    val direct = Module(new DirectFFT[T](
      config = config,
      genMid = genMid,
      genTwiddle = genTwiddleDirect,
      genOutFull = genOutDirect
    ))
    io.out <> direct.io.out

    if (config.n != config.lanes) {
      val biplex = Module(new BiplexFFT[T](config, config.genIn, genMid, genTwiddleBiplex))
      direct.io.in := biplex.io.out
      biplex.io.in <> in
    } else {
      direct.io.in <> in
    }
  } else {
    val direct = Module(new DirectFFT[T](
      config = config,
      genMid = config.genIn,
      genTwiddle = genTwiddleBiplex,
      genOutFull = genMid
    ))

    direct.io.in <> in
    if (config.n != config.lanes) {
      val biplex = Module(new BiplexFFT[T](config, genMid, genOutDirect, genTwiddleDirect))
      biplex.io.in := direct.io.out
      io.out <> biplex.io.out
    } else {
      io.out <> direct.io.out
    }
  }
}


object unscramble {
  def apply[T <: Data : Ring](in: Seq[T], p: Int): Seq[T] = {
    val n = in.size
    val bp = n / p
    val res: Array[Int] = Array.fill(n)(0)
    in.zipWithIndex.grouped(p).zipWithIndex.foreach { case (set, sindex) =>
      set.zipWithIndex.foreach { case ((_, bin), bindex) =>
        if (bp > 1) {
          val p1 = if (sindex / (bp / 2) >= 1) 1 else 0
          val new_index = bit_reverse((sindex % (bp / 2)) * 2 + p1, log2Ceil(bp)) + bit_reverse(bindex, log2Ceil(n))
          res(new_index) = bin
        } else {
          val new_index = bit_reverse(bindex, log2Ceil(n))
          res(new_index) = bin
        }
      }
    }
    res.map(in(_))
  }
}

object bit_reverse {
  // bit reverse a value
  def apply(in: Int, width: Int): Int = {
    var test = in
    var out = 0
    for (i <- 0 until width) {
      if (test / pow(2, width - i - 1) >= 1) {
        out += pow(2, i).toInt
        test -= pow(2, width - i - 1).toInt
      }
    }
    out
  }
}

//This is the object to provide verilog
object FFT extends App {
  // Getopts parses the "Command line arguments for you"  
  def getopts(options : Map[String,String], 
      arguments: List[String]) : (Map[String,String], List[String]) = {
      //This the help
      val usage = """
          |Usage: FFT.FFT [-<option>]
          |
          | Options
          |     IOWidth       [Int]     : Number of bits in the IO. Default 32
          |     binaryPoint   [Int]     : TBD                       Default 16  
          |     n             [Int]     : Number of points in FFT   Default 256  
          |     pipelineDepth [Int]     : TBD                       Default 0  
          |     lanes         [Int]     : Parallel lanes in FFT     Default 256  
          |     quadrature    [Boolean] : TBD                       Default false  
          |     inverse       [Boolean] : Inverse FFT               Default false  
          |     unscrambleOut [Boolean] : Correct output bit-order, Default true
          |                               only functional when 
          |                               (n=lanes)
          |     unscrambleIn [Boolean]  : Correct output bit-order, Default false
          |     h                       : This help 
        """.stripMargin
      val optsWithArg: List[String]=List(
          "-IOWidth",
          "-binaryPoint",
          "-n",
          "-pipelineDepth",
          "-lanes",
          "-quadrature",
          "-inverse",
          "-unscrambleOut",
          "-unscrambleIn"
      )
      //Handling of flag-like options to be defined 
      arguments match {
          case "-h" :: tail => {
              println(usage)
              val (newopts, newargs) = getopts(options, tail)
              sys.exit
              (Map("h"->"") ++ newopts, newargs)
          }
          case option :: value :: tail if optsWithArg contains option => {
             val (newopts, newargs) = getopts(
                 options++Map(option.replace("-","") -> value), tail
             )
             (newopts, newargs)
          }
            case argument :: tail => {
               val (newopts, newargs) = getopts(options,tail)
               (newopts, argument.toString +: newargs)
            }
          case Nil => (options, arguments)
      }
  }
   
  // Default options
  val defaultoptions : Map[String,String]=Map(
      "IOWidth"->"32",
      "binaryPoint"->"16",
      "n"->"256",
      "pipelineDepth"->"0",
      "lanes"->"256",
      "quadrature"->"false",
      "inverse"->"false",
      "unscrambleOut"->"true",
      "unscrambleIn"->"false"
      ) 
  // Parse the options
  val (options,arguments)= getopts(defaultoptions,args.toList)

  val fixedConfig = FixedFFTConfig(
    IOWidth       = options("IOWidth").toInt,
    binaryPoint   = options("binaryPoint").toInt,
    n             = options("n").toInt, // n-point FFT
    pipelineDepth = options("pipelineDepth").toInt,
    lanes         = options("lanes").toInt, // number of input
    quadrature    = options("quadrature").toBoolean,
    inverse       = options("inverse").toBoolean, // do inverse fft when true
    unscrambleOut = options("unscrambleOut").toBoolean, //  correct output bit-order, only functional when (n=lanes)
    unscrambleIn  = options("unscrambleIn").toBoolean // accept srambled input
  )

  chisel3.Driver.execute(arguments.toArray, () => new FFT(fixedConfig) )
}

