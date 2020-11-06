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
    // TODO: Measure BW if using sbus instead of pbus
    val (hostWidget, hostWidgetMemNode) = TLTSIHostWidget.attach(TSIHostWidgetAttachParams(ps, pbus))

    hostWidget
  }

  val tsiMemTLNodes = (p(PeripheryTSIHostKey) zip tsiHostWidgetNodes).map { case (params, node) =>
    val device = new MemoryDevice

    val idBits = 4 // copying the ExtMem
    val beatBytes = 8 // taken from the MemoryBusKey

    val slaveParams = TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address            = Seq(AddressSet(params.targetBaseAddress, params.targetSize - 1)),
        resources          = device.reg,
        regionType         = RegionType.UNCACHED, // cacheable
        executable         = true,
        supportsGet        = TransferSizes(1, p(CacheBlockBytes)),
        supportsPutFull    = TransferSizes(1, p(CacheBlockBytes)),
        supportsPutPartial = TransferSizes(1, p(CacheBlockBytes)))),
      beatBytes = beatBytes)

    val managerNode = TLManagerNode(Seq(slaveParams))

    (managerNode
      := TLBuffer()
      := TLSourceShrinker(1 << idBits)
      := TLWidthWidget(beatBytes)
      := node.externalClientNode)

    managerNode
  }

  // i/o to the outside world (to the host memory)
  val tsiTLMem = tsiMemTLNodes.map { tlnodes => InModuleBody { tlnodes.makeIOs() } }

  // serial i/o to the outside world (to the DUT serial link)
  val tsiSerial = tsiHostWidgetNodes.map { node =>
    val sink = node.ioNode.makeSink()(p)
    InModuleBody {
      sink.makeIO()
    }
  }
}
