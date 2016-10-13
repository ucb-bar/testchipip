package testchipip

import chisel3._
import chisel3.util._

class ResetSync(c: Clock, lat: Int = 2) extends Module(_clock = c) {
  val io = new Bundle {
    val reset = Bool(INPUT)
    val reset_sync = Bool(OUTPUT)
  }
  io.reset_sync := ShiftRegister(io.reset,lat)
}
object ResetSync {
  def apply(r: Bool, c: Clock): Bool =  {
    val sync = Module(new ResetSync(c,2))
    sync.suggestName("resetSyncInst")
    sync.io.reset := r
    sync.io.reset_sync
  }
}

// [ben] Like Chisel3 Counter, but can take in a clock and reset
class CounterMC(val n: Int, c: Clock, r: Bool) extends Module(_clock = c, _reset = r) {
  val io = new Bundle
  val counter = new Counter(n)
  def inc(): Bool = counter.inc()
}

object CounterMC {
  def apply(n: Int, c: Clock, r: Bool): CounterMC = Module(new CounterMC(n, c, r))
}

// [ben] Copies Chisel3 Queue, but can take in a clock and reset
class QueueMC[T <: Data](gen: T,
                       val entries: Int,
                       pipe: Boolean = false,
                       flow: Boolean = false,
                       c: Clock = null,
                       r: Bool = null)
extends Module(Option(c), Option(r)) {

  val io = new QueueIO(gen, entries)

  val ram = Mem(entries, gen)
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = Reg(init=Bool(false))

  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = Wire(init=io.enq.fire())
  val do_deq = Wire(init=io.deq.fire())

  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when (do_deq) {
    deq_ptr.inc()
  }
  when (do_enq != do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(deq_ptr.value)

  if (flow) {
    when (io.enq.valid) { io.deq.valid := Bool(true) }
    when (empty) {
      io.deq.bits := io.enq.bits
      do_deq := Bool(false)
      when (io.deq.ready) { do_enq := Bool(false) }
    }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := Bool(true) }
  }

  val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Cat(maybe_full && ptr_match, ptr_diff)
  } else {
    io.count := Mux(ptr_match,
                    Mux(maybe_full,
                      UInt(entries), UInt(0)),
                    Mux(deq_ptr.value > enq_ptr.value,
                      UInt(entries) + ptr_diff, ptr_diff))
  }
}

object QueueMC
{
  /** Create a queue and supply a DecoupledIO containing the product. */
  def apply[T <: Data](
      enq: ReadyValidIO[T],
      entries: Int = 2,
      pipe: Boolean = false,
      flow: Boolean = false,
      c: Clock = null,
      r: Bool = null): DecoupledIO[T] = {
    val q = Module(new QueueMC(enq.bits.cloneType, entries, pipe, flow, c, r))
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    TransitName(q.io.deq, q)
  }

  /** Create a queue and supply a IrrevocableIO containing the product.
    * Casting from Decoupled is safe here because we know the Queue has
    * Irrevocable semantics; we didn't want to change the return type of
    * apply() for backwards compatibility reasons.
    */
  def irrevocable[T <: Data](
      enq: ReadyValidIO[T],
      entries: Int = 2,
      pipe: Boolean = false,
      flow: Boolean = false,
      c: Clock = null,
      r: Bool = null): IrrevocableIO[T] = {
    val deq = apply(enq, entries, pipe, flow, c, r)
    val irr = Wire(new IrrevocableIO(deq.bits))
    irr.bits := deq.bits
    irr.valid := deq.valid
    deq.ready := irr.ready
    irr
  }
}
