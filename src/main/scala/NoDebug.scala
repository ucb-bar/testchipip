package testchipip

import chisel3._
import freechips.rocketchip.chip.HasCoreplexRISCVPlatform
import freechips.rocketchip.coreplex.CoreplexRISCVPlatform
import freechips.rocketchip.config.Parameters

trait HasNoDebug extends HasCoreplexRISCVPlatform {
  val coreplex: CoreplexRISCVPlatform
}

trait HasNoDebugModuleImp {
  implicit val p: Parameters
  val outer: HasNoDebug
  val debugIO = outer.coreplex.module.io.debug
  val clock: Clock
  val reset: Bool

  debugIO.dmi.req.valid := false.B
  debugIO.dmi.resp.ready := false.B
  debugIO.dmiClock := clock
  debugIO.dmiReset := reset
}
