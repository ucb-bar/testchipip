package testchipip

import Chisel._

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
