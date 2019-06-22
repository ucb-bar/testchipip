package testchipip

import chisel3._
import chisel3.core.Reset
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.config.Parameters

trait HasNoDebug extends HasPeripheryDebug { this: BaseSubsystem =>
}

trait HasNoDebugModuleImp {
  implicit val p: Parameters
  val outer: HasNoDebug
  val debugIO = outer.debug.module.io.dmi
  val clock: Clock
  val reset: Reset

  debugIO.foreach { dbg =>
    dbg.dmi.req.valid := false.B
    dbg.dmi.resp.ready := false.B
    dbg.dmiClock := clock
    dbg.dmiReset := reset.toBool
  }
}
