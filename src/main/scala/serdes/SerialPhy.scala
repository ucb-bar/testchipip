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

class DecoupledSerialPhy(phyParams: SerialPhyParams) extends RawModule {
  val io = IO(new Bundle {
    val outer_clock = Input(Clock())
    val outer_reset = Input(Bool())
    val inner_clock = Input(Clock())
    val inner_reset = Input(Bool())
    val outer_ser = new DecoupledPhitIO(phyParams.phitWidth)
    val inner_ser = Flipped(new DecoupledFlitIO(phyParams.flitWidth))
  })

  val out_async = Module(new AsyncQueue(new Flit(phyParams.flitWidth), AsyncQueueParams(depth=phyParams.asyncQueueSz)))
  out_async.io.enq_clock := io.inner_clock
  out_async.io.enq_reset := io.inner_reset
  out_async.io.deq_clock := io.outer_clock
  out_async.io.deq_reset := io.outer_reset
  out_async.io.enq <> io.inner_ser.out
  io.outer_ser.out <> withClockAndReset(io.outer_clock, io.outer_reset) { FlitToPhit(out_async.io.deq, phyParams.phitWidth) }

  val in_async = Module(new AsyncQueue(new Flit(phyParams.flitWidth), AsyncQueueParams(depth=phyParams.asyncQueueSz)))
  in_async.io.enq_clock := io.outer_clock
  in_async.io.enq_reset := io.outer_reset
  in_async.io.deq_clock := io.inner_clock
  in_async.io.deq_reset := io.inner_reset
  io.inner_ser.in <> in_async.io.deq
  in_async.io.enq <> withClockAndReset(io.outer_clock, io.outer_reset) { PhitToFlit(io.outer_ser.in, phyParams.flitWidth) }
}

class CreditedSerialPhy(phyParams: SerialPhyParams) extends RawModule {
  val io = IO(new Bundle {
    val incoming_clock = Input(Clock())
    val incoming_reset = Input(Bool())
    val outgoing_clock = Input(Clock())
    val outgoing_reset = Input(Bool())
    val inner_clock = Input(Clock())
    val inner_reset = Input(Bool())

    val outer_ser = new CreditedPhitIO(phyParams.phitWidth)
    val inner_ser = Flipped(new DecoupledFlitIO(phyParams.flitWidth))
  })

  val out_async = Module(new AsyncQueue(new Flit(phyParams.flitWidth)))
  out_async.io.enq_clock := io.inner_clock
  out_async.io.enq_reset := io.inner_reset
  out_async.io.deq_clock := io.outgoing_clock
  out_async.io.deq_reset := io.outgoing_reset
  out_async.io.enq <> io.inner_ser.out

  // This is just a gray-coded counter, not a queue
  val out_credits = Module(new AsyncQueue(Bool(), AsyncQueueParams(depth=phyParams.asyncQueueSz)))
  out_credits.io.enq_clock := io.outgoing_clock
  out_credits.io.enq_reset := io.outgoing_reset
  out_credits.io.deq_clock := io.incoming_clock
  out_credits.io.deq_reset := io.incoming_reset

  val in_async = Module(new AsyncQueue(new Flit(phyParams.flitWidth)))
  in_async.io.enq_clock := io.incoming_clock
  in_async.io.enq_reset := io.incoming_reset
  in_async.io.deq_clock := io.inner_clock
  in_async.io.deq_reset := io.inner_reset
  io.inner_ser.in <> in_async.io.deq

  // This is just a gray-coded counter, not a queue
  val in_credits = Module(new AsyncQueue(Bool(), AsyncQueueParams(depth=phyParams.asyncQueueSz)))
  in_credits.io.enq_clock := io.inner_clock
  in_credits.io.enq_reset := io.inner_reset
  in_credits.io.deq_clock := io.outgoing_clock
  in_credits.io.deq_reset := io.outgoing_reset

  // data out
  val out_flit = Wire(Decoupled(new Flit(phyParams.flitWidth)))
  out_flit.valid := out_credits.io.enq.ready && out_async.io.deq.valid
  out_credits.io.enq.valid := out_async.io.deq.valid && out_flit.ready
  out_async.io.deq.ready := out_credits.io.enq.ready && out_flit.ready
  out_flit.bits := out_async.io.deq.bits

  out_credits.io.enq.bits := DontCare // Should cause most of the AsyncQueue to DCE away
  val out_phit = withClockAndReset(io.outgoing_clock, io.outgoing_reset) { FlitToPhit(out_flit, phyParams.phitWidth) }
  out_phit.ready := true.B
  io.outer_ser.phit_out.valid := out_phit.valid
  io.outer_ser.phit_out.bits := out_phit.bits

  // credit in
  out_credits.io.deq.ready := io.outer_ser.credit_in

  // data in
  val in_flit = withClockAndReset(io.incoming_clock, io.incoming_reset) { PhitToFlit(io.outer_ser.phit_in, phyParams.flitWidth) }
  val in_buffer = withClockAndReset(io.incoming_clock, io.incoming_reset) { Module(new Queue(new Flit(phyParams.flitWidth), phyParams.asyncQueueSz)) }
  in_buffer.io.enq.valid := in_flit.valid
  in_buffer.io.enq.bits := in_flit.bits
  in_async.io.enq <> in_buffer.io.deq

  // credit out
  in_credits.io.enq.valid := in_buffer.io.deq.fire
  in_credits.io.enq.bits := DontCare
  in_credits.io.deq.ready := true.B
  io.outer_ser.credit_out := in_credits.io.deq.valid
}
