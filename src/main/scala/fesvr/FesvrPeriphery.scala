package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/**
 * Configuration parameter to configure the FESVR Widget
 */
case object PeripheryFESVRKey extends Field[FESVRParams]

/**
 * Trait to create a FESVR Widget
 */
trait HasPeripheryFESVRWidget { this: BaseSubsystem =>
  private val portName = "FESVR-Widget"

  val fesvrWidget = LazyModule(new TLFESVRWidget(sbus.beatBytes))

  // the mmio (a manager) needs to connect to the system bus
  // note: toVariableWidthSlave adds extra logic so that the mmio (which can have a
  //       variety of widths according to the registers) can connect correctly
  sbus.toVariableWidthSlave(Some(portName)) { fesvrWidget.mmioNode }
  // this client is the master on the system bus
  sbus.fromPort(Some(portName))() :=* fesvrWidget.externalClientNode
}

/**
 * Trait to create a top-level IO that connects the (inner) FESVR Widget to the
 * outside world
 */
trait HasPeripheryFESVRWidgetImp extends LazyModuleImp {
  val outer: HasPeripheryFESVRWidget
  implicit val p: Parameters

  // i/o out to the outside world
  val fesvrIO = IO(new FESVRWidgetIO(p(PeripheryFESVRKey).serialIfWidth)(p))

  // connect to inner modules to the outside (punch through to top)
  fesvrIO.serial <> outer.fesvrWidget.module.io.serial

  def connectLoopback(queueDepth: Int = 64) {
    fesvrIO.serial.in <> Queue(fesvrIO.serial.out, queueDepth)
  }

  def connectDebug(other: SerialIO) {
    require (p(PeripheryFESVRKey).bypassMMIO == true)
    fesvrIO.debug <> other
  }

  def tieOffFESVR() = {
    fesvrIO.serial.in.bits := 0.U
    fesvrIO.serial.in.valid := 0.U
    fesvrIO.serial.out.ready := 0.U
  }
}
