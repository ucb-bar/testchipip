package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}
import freechips.rocketchip.util._

class ResetStretcher(cycles: Int) extends Module {
  val io = IO(new Bundle {
    val reset_out = Output(Bool())
  })
  val n = log2Ceil(cycles)
  val count = Module(new AsyncResetRegVec(w=n, init=0))
  val resetout = Module(new AsyncResetRegVec(w=1, init=1))
  count.io.en := resetout.io.q
  count.io.d := count.io.q + 1.U
  resetout.io.en := resetout.io.q
  resetout.io.d := count.io.q < (cycles-1).U
  io.reset_out := resetout.io.q.asBool
}

object ResetStretcher {
  def apply(clock: Clock, reset: Reset, cycles: Int): Reset = {
    withClockAndReset(clock, reset) {
      val stretcher = Module(new ResetStretcher(cycles))
      stretcher.io.reset_out
    }
  }
}
