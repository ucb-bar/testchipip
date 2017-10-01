package testchipip

import chisel3._
import chisel3.core.Reset
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.config.Parameters

trait HasNoDebug extends HasPeripheryDebug {
}

trait HasNoDebugModuleImp {
  implicit val p: Parameters
  val outer: HasNoDebug
  val debugIO = outer.debug.module.io.dmi
  val clock: Clock
  val reset: Reset

  debugIO.dmi.req.valid := false.B
  debugIO.dmi.resp.ready := false.B
  debugIO.dmiClock := clock
  debugIO.dmiReset := reset.toBool
}
