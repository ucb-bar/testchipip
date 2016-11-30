package testchipip

import chisel3.util.ShiftRegister
import chisel3.util.Enum
import chisel3.util.Counter
import util.WideCounter
import chisel3._
import uncore.tilelink._
import cde.Parameters

class ResetSync(c: Clock, lat: Int = 2) extends Module(_clock = c) {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val reset_sync = Output(Bool())
  })
  io.reset_sync := ShiftRegister(io.reset,lat)
}

object ResetSync {
  def apply(r: Bool, c: Clock): Bool = {
    val sync = Module(new ResetSync(c,2))
    sync.suggestName("resetSyncInst")
    sync.io.reset := r
    sync.io.reset_sync
  }
}

// As WideCounter, but it's a module so it can take arbitrary clocks
class WideCounterModule(w: Int, inc: UInt = UInt(1), reset: Boolean = true, clockSignal: Clock = null, resetSignal: Bool = null)
    extends Module(Option(clockSignal), Option(resetSignal)) {
  val io = IO(new Bundle {
    val value = Output(UInt(width = w))
  })
  io.value := WideCounter(w, inc, reset)
}

object WideCounterModule {
  def apply(w: Int, c: Clock, r: Bool) = {
    val counter = Module(new WideCounterModule(w, clockSignal = c, resetSignal = r))
    counter.io.value
  }
  def apply(w: Int, c: Clock) = {
    val counter = Module(new WideCounterModule(w, clockSignal = c))
    counter.io.value
  }
}

/**
 * Simple widget to apply a sequence of puts to a port
 * @param s a Seq of Puts
 */
class PutSeqDriver(val s: Seq[Tuple2[BigInt,Int]])(implicit p: Parameters) extends Driver()(p) {

  val (s_idle :: s_put_req :: s_put_resp :: s_done :: Nil) = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val n = s.size
  val puts = Vec(s.map { case (a, d) =>
    val addr = UInt(a)
    val beatAddr = addr(tlBeatAddrBits+tlByteAddrBits-1,tlByteAddrBits)
    val blockAddr = addr(tlBlockAddrBits+tlBeatAddrBits+tlByteAddrBits-1,tlBeatAddrBits+tlByteAddrBits)
    Put(UInt(0), blockAddr, beatAddr, UInt(d))
  })

  val (put_cnt, put_done) = Counter(state === s_put_resp && io.mem.grant.valid, n)

  io.mem.acquire.valid := state === s_put_req
  io.mem.acquire.bits := puts(put_cnt)
  io.mem.grant.ready := state === s_put_resp

  when (state === s_idle && io.start) { state := s_put_req }
  when (state === s_put_req && io.mem.acquire.ready) { state := s_put_resp }
  when (state === s_put_resp && io.mem.grant.valid) {
    state := Mux(put_done, s_done, s_put_req)
  }

  io.finished := (state === s_done)

}

