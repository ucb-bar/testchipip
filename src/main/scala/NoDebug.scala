package testchipip

import chisel3._
import rocketchip.{HasTopLevelNetworks, HasTopLevelNetworksModule}
import coreplex.CoreplexRISCVPlatform
import config.Parameters

trait NoDebug extends HasTopLevelNetworks {
  val coreplex: CoreplexRISCVPlatform
}

trait NoDebugModule extends HasTopLevelNetworksModule {
  implicit val p: Parameters
  val outer: NoDebug
  val debugIO = outer.coreplex.module.io.debug
  val clock: Clock
  val reset: Bool

  debugIO.dmi.req.valid := false.B
  debugIO.dmi.resp.ready := false.B
  debugIO.dmiClock := clock
  debugIO.dmiReset := reset
}
