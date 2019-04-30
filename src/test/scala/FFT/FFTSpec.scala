
package FFT

import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import breeze.math.{Complex}
import breeze.signal.{fourierTr}
import breeze.linalg._
import chisel3._
import chisel3.experimental._
import chisel3.util._
import chisel3.iotesters._
import firrtl_interpreter.InterpreterOptions
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import chisel3.iotesters.{PeekPokeTester, TesterOptionsManager}

// comment when using FixedPoint, uncomment for DspReal
// import dsptools.numbers.implicits._

import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math.{pow, abs, round}
import org.scalatest.Tag
import dspjunctions._
import dspblocks._

import craft._
import dsptools._
import freechips.rocketchip.config.Parameters

class FFTSpec extends FlatSpec with Matchers {
  behavior of "FFT"

  val config = FixedFFTConfig(
    IOWidth = 32,
    binaryPoint = 16,
    n = 256, // n-point FFT
    pipelineDepth = 0,
    lanes = 256, // number of input
    quadrature = false,
    inverse = false, // do inverse fft when true
    unscrambleOut = true, //  correct output bit-order, only functional when (n=lanes)
    unscrambleIn = false // accept srambled input
  )

  def getTone(numSamples: Int, f: Double): Seq[Complex] = {
    // uncomment to scale input tone
    //(0 until numSamples).map(i => pow(2, -(numSamples+1))*Complex(math.cos(2 * math.Pi * f * i), math.sin(2 * math.Pi * f * i)))
    (0 until numSamples).map(i => Complex(math.cos(2 * math.Pi * f * i), math.sin(2 * math.Pi * f * i)))
  }

  it should "fixed fft" in {
    // Change this to a sine wave
    val tone = getTone(config.n, 0.1234)
    var sampleSeq = Seq.empty[Complex]
    for (i <- 0 until config.n) {
      sampleSeq = sampleSeq :+ tone(i)
    }
    FixedFFTTester(config, sampleSeq) should be (true)
  }
}
