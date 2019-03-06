package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._

/**
 * Configuration parameter to configure the TSI Host Widget
 */
case object PeripheryTSIHostKey extends Field[TSIHostParams]

/**
 * Trait to create a TSI Host Widget
 */
trait HasPeripheryTSIHostWidget { this: BaseSubsystem =>
  val tsiHostWidget = TLTSIHostWidget.attach(TSIHostWidgetAttachParams(p(PeripheryTSIHostKey), sbus, memBuses))
}

/**
 * Trait to create a top-level IO that connects the (inner) TSI Host Widget to the
 * outside world
 */
trait HasPeripheryTSIHostWidgetImp extends LazyModuleImp {
  val outer: HasPeripheryTSIHostWidget
  implicit val p: Parameters

  // i/o out to the outside world
  val tsiHostIO = IO(new TSIHostWidgetIO(p(PeripheryTSIHostKey).serialIfWidth))

  // connect to inner modules to the outside (punch through to top)
  tsiHostIO.serial <> outer.tsiHostWidget.module.io.serial
}
