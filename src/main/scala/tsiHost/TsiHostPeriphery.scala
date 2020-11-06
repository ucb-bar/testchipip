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
 * TODO: Have the memory node IOs be punched out... then connect it in the harness
 */

/**
 * Trait to create a set of TSI Host Widgets
 */
trait HasPeripheryTSIHostWidget { this: BaseSubsystem =>
  val tsiHostWidgetNodes = p(PeripheryTSIHostKey).map { ps =>
    // TODO: Measure BW if using sbus instead of pbus
    val (hostWidget, hostWidgetMemNode) = TLTSIHostWidget.attach(TSIHostWidgetAttachParams(ps, pbus))

    hostWidget
  }

  // i/o to the outside world
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

  val tsiMem = tsiMemTLNodes.map { tlnodes => InModuleBody { tlnodes.makeIOs() } }
}

/**
 * Trait to create a top-level IO that connects the (inner) TSI Host Widget to the
 * outside world
 */
trait HasPeripheryTSIHostWidgetImp extends LazyModuleImp {
  val outer: HasPeripheryTSIHostWidget
  implicit val p: Parameters

  // i/o out to the outside world
  val tsi = outer.tsiHostWidgetNodes.map { case n =>
    val tsiio = IO(new TSIHostWidgetIO(n.params.serialIfWidth))
    val innerTsiSink = n.ioNode.makeSink()(p)

    tsiio <> innerTsiSink.bundle
  }
}
