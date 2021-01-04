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
 * Configuration parameter to configure a set of TSI Host Widgets
 */
case object PeripheryTSIHostKey extends Field[Seq[TSIHostParams]](Nil)

/**
 * Trait to create a set of TSI Host Widgets
 */
trait HasPeripheryTSIHostWidget { this: BaseSubsystem =>
  val tsiHostWidgetNodes = p(PeripheryTSIHostKey).map { ps =>
    val (hostWidget, hostWidgetMemNode) = TLTSIHostWidget.attach(TSIHostWidgetAttachParams(ps, pbus))

    hostWidget
  }

  val tsiMemTLNodes = (p(PeripheryTSIHostKey) zip tsiHostWidgetNodes).map { case (params, node) =>
    val device = new MemoryDevice

    val slaveParams = TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address            = Seq(AddressSet(params.targetMasterPortParams.base, params.targetMasterPortParams.size - 1)),
        resources          = device.reg,
        regionType         = RegionType.UNCACHED, // cacheable
        executable         = true,
        supportsGet        = TransferSizes(1, p(CacheBlockBytes)),
        supportsPutFull    = TransferSizes(1, p(CacheBlockBytes)),
        supportsPutPartial = TransferSizes(1, p(CacheBlockBytes)))),
      beatBytes = params.targetMasterPortParams.beatBytes)

    val managerNode = TLManagerNode(Seq(slaveParams))

    (managerNode
      := TLBuffer()
      := TLSourceShrinker(1 << params.targetMasterPortParams.idBits)
      := TLWidthWidget(params.targetMasterPortParams.beatBytes)
      := node.externalClientNode)

    managerNode
  }

  // memory i/o to the outside world (to the host memory)
  val tsiTLMem = tsiMemTLNodes.map { tlnodes => InModuleBody { tlnodes.makeIOs() } }

  // serial i/o to the outside world (to the DUT serial link)
  val tsiSerial = tsiHostWidgetNodes.map { node =>
    val sink = node.ioNode.makeSink()(p)
    InModuleBody {
      sink.makeIO()
    }
  }
}
