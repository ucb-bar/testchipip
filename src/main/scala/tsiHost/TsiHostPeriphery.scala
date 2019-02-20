package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/**
 * Configuration parameter to configure the TSI Host Widget
 */
case object PeripheryTSIHostKey extends Field[TSIHostParams]

/**
 * Trait to create a TSI Host Widget
 */
trait HasPeripheryTSIHostWidget { this: BaseSubsystem =>
  private val portName = "TSI-Host-Widget"

  val tsiHostWidget = LazyModule(new TLTSIHostWidget(sbus.beatBytes))

  // the mmio (a manager) needs to connect to the system bus
  // note: toVariableWidthSlave adds extra logic so that the mmio (which can have a
  //       variety of widths according to the registers) can connect correctly
  sbus.toVariableWidthSlave(Some(portName)) { tsiHostWidget.mmioNode }
  // this client is the master on the system bus
  sbus.fromPort(Some(portName))() :=* tsiHostWidget.externalClientNode
}

/**
 * Trait to create a top-level IO that connects the (inner) TSI Host Widget to the
 * outside world
 */
trait HasPeripheryTSIHostWidgetImp extends LazyModuleImp {
  val outer: HasPeripheryTSIHostWidget
  implicit val p: Parameters

  // i/o out to the outside world
  val tsiHostIO = IO(new TSIHostWidgetIO(p(PeripheryTSIHostKey).serialIfWidth)(p))

  // connect to inner modules to the outside (punch through to top)
  tsiHostIO.serial <> outer.tsiHostWidget.module.io.serial

  def connectLoopback(queueDepth: Int = 64) {
    tsiHostIO.serial.in <> Queue(tsiHostIO.serial.out, queueDepth)
  }

  def connectDebug(other: SerialIO) {
    require (p(PeripheryTSIHostKey).bypassMMIO == true)
    tsiHostIO.debug <> other
  }

  def tieOffTSIHost() = {
    tsiHostIO.serial.in.bits := 0.U
    tsiHostIO.serial.in.valid := 0.U
    tsiHostIO.serial.out.ready := 0.U
  }
}
