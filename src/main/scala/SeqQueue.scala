package testchipip

import chisel3._
import chisel3.util._
import cde.{Parameters, Field}

class SeqQueue[T <: Data](data: => T, entries: Int)(implicit p: Parameters) extends Module {
  val io = IO(new QueueIO(data, entries))

  val do_flow = Wire(Bool())
  val do_enq = io.enq.fire() && !do_flow
  val do_deq = io.deq.fire() && !do_flow

  val maybe_full = RegInit(false.B)
  val enq_ptr = RegInit(UInt(0, log2Up(entries)))
  val deq_ptr = RegInit(UInt(0, log2Up(entries)))
  val enq_wrap = enq_ptr === (entries - 1).U
  val deq_wrap = deq_ptr === (entries - 1).U
  when (do_enq) { enq_ptr := Mux(enq_wrap, 0.U, enq_ptr + 1.U) }
  when (do_deq) { deq_ptr := Mux(deq_wrap, 0.U, deq_ptr + 1.U) }
  when (do_enq =/= do_deq) { maybe_full := do_enq }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val atLeastTwo = full || enq_ptr - deq_ptr >= 2.U
  do_flow := empty && io.deq.ready

  val ram = SeqMem(entries, data)
  when (do_enq) { ram.write(enq_ptr, io.enq.bits) }

  val ren = io.deq.ready && (atLeastTwo || !io.deq.valid && !empty)
  val ram_out_valid = RegNext(ren)
  val raddr = Mux(io.deq.valid, Mux(deq_wrap, 0.U, deq_ptr + 1.U), deq_ptr)

  io.enq.ready := !full
  io.deq.valid := Mux(empty, io.enq.valid, ram_out_valid)
  io.deq.bits  := Mux(empty, io.enq.bits, ram.read(raddr, ren))

  val ptr_diff = enq_ptr - deq_ptr
  if (isPow2(entries)) {
    io.count := Cat(maybe_full && ptr_match, ptr_diff)
  } else {
    io.count := Mux(ptr_match,
                    Mux(maybe_full,
                      entries.U, 0.U),
                    Mux(deq_ptr > enq_ptr,
                      entries.U + ptr_diff, ptr_diff))
  }
}

object SeqQueue {
  def apply[T <: Data](
      enq: ReadyValidIO[T],
      entries: Int = 2)
     (implicit p: Parameters) = {
    val queue = Module(new SeqQueue(enq.bits, entries))
    queue.io.enq <> enq
    queue.io.deq
  }
}
