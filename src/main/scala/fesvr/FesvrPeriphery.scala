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
  private val address = BigInt(0x10016000) // Address where the MMIO registers start
  private val portName = "FESVR-Widget"

  val fesvrWidget = LazyModule(new TLFESVRWidget(sbus.beatBytes))

  // TODO: Check that this is correct
  sbus.toVariableWidthSlave(Some(portName)) { fesvrWidget.mmioNode }
  sbus.fromPort(Some(portName))() :=* fesvrWidget.extTLNode
}

/**
 * Trait to create a top-level IO that connects the (inner) FESVR Widget to the
 * outside world
 */
trait HasPeripheryFESVRWidgetImp extends LazyModuleImp {
  val outer: HasPeripheryFESVRWidget
  implicit val p: Parameters

  // i/o out to the outside world
  val fesvrIO = IO(new FESVRWidgetIO(p(PeripheryFESVRKey).serialIfWidth))

  // connect to inner modules to the outside (punch through to top)
  fesvrIO <> outer.fesvrWidget.module.io

  def connectLoopback(queueDepth: Int = 64) {
    fesvrIO.in <> Queue(fesvrIO.out, queueDepth)
  }
}
