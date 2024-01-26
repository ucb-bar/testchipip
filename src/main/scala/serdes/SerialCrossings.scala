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

class CreditedSerialCrossing(w: Int, queueSz: Int) extends RawModule {
  val io = IO(new Bundle {
    val incoming_clock = Input(Clock())
    val incoming_reset = Input(Bool())
    val outgoing_clock = Input(Clock())
    val outgoing_reset = Input(Bool())
    val inner_clock = Input(Clock())
    val inner_reset = Input(Bool())

    val outer_ser = new CreditedSerialIO(w)
    val inner_ser = Flipped(new DecoupledSerialIO(w))
  })

  val out_async = Module(new AsyncQueue(UInt(w.W)))
  out_async.io.enq_clock := io.inner_clock
  out_async.io.enq_reset := io.inner_reset
  out_async.io.deq_clock := io.outgoing_clock
  out_async.io.deq_reset := io.outgoing_reset
  out_async.io.enq <> io.inner_ser.out

  val out_credits = Module(new AsyncQueue(Bool(), AsyncQueueParams(depth=queueSz)))
  out_credits.io.enq_clock := io.outgoing_clock
  out_credits.io.enq_reset := io.outgoing_reset
  out_credits.io.deq_clock := io.incoming_clock
  out_credits.io.deq_reset := io.incoming_reset

  val in_async = Module(new AsyncQueue(UInt(w.W), AsyncQueueParams(depth=queueSz)))
  in_async.io.enq_clock := io.incoming_clock
  in_async.io.enq_reset := io.incoming_reset
  in_async.io.deq_clock := io.inner_clock
  in_async.io.deq_reset := io.inner_reset
  io.inner_ser.in <> in_async.io.deq

  val in_credits = Module(new AsyncQueue(Bool(), AsyncQueueParams(depth=queueSz)))
  in_credits.io.enq_clock := io.inner_clock
  in_credits.io.enq_reset := io.inner_reset
  in_credits.io.deq_clock := io.outgoing_clock
  in_credits.io.deq_reset := io.outgoing_reset

  // data out
  out_credits.io.enq.valid := out_async.io.deq.valid
  out_credits.io.enq.bits := DontCare // Should cause most of the AsyncQueue to DCE away
  out_async.io.deq.ready := out_credits.io.enq.ready
  io.outer_ser.out.valid := out_async.io.deq.fire
  io.outer_ser.out.bits := out_async.io.deq.bits

  // credit in
  out_credits.io.deq.ready := io.outer_ser.credit_in

  // data in
  in_async.io.enq.valid := io.outer_ser.in.valid
  in_async.io.enq.bits := io.outer_ser.in.bits
  withClockAndReset(io.incoming_clock, io.incoming_reset) {
    when (io.outer_ser.in.valid) { assert(in_async.io.enq.ready, "Credited flow control broke") }
  }

  // redit out
  in_credits.io.enq.valid := in_async.io.deq.fire
  in_credits.io.enq.bits := DontCare
  withClockAndReset(io.inner_clock, io.inner_reset) {
    when (in_async.io.deq.fire) { assert(in_credits.io.enq.ready, "Credited flow control broke") }
  }
  in_credits.io.deq.ready := true.B
  io.outer_ser.credit_out := in_credits.io.deq.valid
}
