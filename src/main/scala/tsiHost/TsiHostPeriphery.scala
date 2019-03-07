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
case object PeripheryTSIHostKey extends Field[Seq[TSIHostParams]]

/**
 * Trait to create a TSI Host Widget
 */
trait HasPeripheryTSIHostWidget { this: BaseSubsystem =>
  val tsiHostWidgetNodes = p(PeripheryTSIHostKey).map { ps =>
    TLTSIHostWidget.attach(TSIHostWidgetAttachParams(ps, sbus, mbus))
  }
}

/**
 * Trait to create a top-level IO that connects the (inner) TSI Host Widget to the
 * outside world
 */
trait HasPeripheryTSIHostWidgetImp extends LazyModuleImp {
  val outer: HasPeripheryTSIHostWidget
  implicit val p: Parameters

  // i/o out to the outside world
  val tsi = outer.tsiHostWidgetNodes.map { case n => IO(new TSIHostWidgetIO(n.params.serialIfWidth)) }
}
