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

/**
 * DspTester for FFT
 *
 * Run each trial in @trials
 *
 */
class FFTTester[T <: chisel3.Data](c: FFT[T], samples: Seq[Complex]) extends DspTester(c) {
  val maxCyclesWait = 50

  // Supress peek and poke output
  updatableDspVerbose.value = false

	for (i <- 0 until samples.size) {
		  	poke(c.io.in.bits(i), samples(i))
		}
		poke(c.io.in.valid, true.B)
		step(1)

		val expectedResult = fourierTr(DenseVector(samples.toArray)).toArray
		for (i <- 0 until samples.size) {
			fixTolLSBs.withValue(8) {
				expect(c.io.out.bits(i), expectedResult(i))
			}
		}
	}

/**
 * Convenience function for running tests
 */
object FixedFFTTester {
  def apply(config: FixedFFTConfig, samples: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new FFT(config)) {
      c => new FFTTester(c, samples)
    }
  }
}
