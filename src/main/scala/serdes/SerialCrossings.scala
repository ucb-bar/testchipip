package testchipip.serdes

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._

class DecoupledSerialCrossing(w: Int, queueSz: Int) extends RawModule {
  val io = IO(new Bundle {
    val outer_clock = Input(Clock())
    val outer_reset = Input(Bool())
    val inner_clock = Input(Clock())
    val inner_reset = Input(Bool())
    val outer_ser = new DecoupledSerialIO(w)
    val inner_ser = Flipped(new DecoupledSerialIO(w))
  })

  val out_async = Module(new AsyncQueue(UInt(w.W), AsyncQueueParams(depth=queueSz)))
  out_async.io.enq_clock := io.inner_clock
  out_async.io.enq_reset := io.inner_reset
  out_async.io.deq_clock := io.outer_clock
  out_async.io.deq_reset := io.outer_reset
  out_async.io.enq <> io.inner_ser.out
  io.outer_ser.out <> out_async.io.deq

  val in_async = Module(new AsyncQueue(UInt(w.W), AsyncQueueParams(depth=queueSz)))
  in_async.io.enq_clock := io.outer_clock
  in_async.io.enq_reset := io.outer_reset
  in_async.io.deq_clock := io.inner_clock
  in_async.io.deq_reset := io.inner_reset
  io.inner_ser.in <> in_async.io.deq
  in_async.io.enq <> io.outer_ser.in
}
