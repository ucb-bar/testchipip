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
  private val portName = "tsi-host-widget"

  val tsiHostWidget = LazyModule(new TLTSIHostWidget(sbus.beatBytes))

  // the mmio (a manager) needs to connect to the system bus
  // note: toVariableWidthSlave adds extra logic so that the mmio (which can have a
  //       variety of widths according to the registers) can connect correctly
  sbus.toVariableWidthSlave(Some(portName)) { tsiHostWidget.mmioNode }

  // connect the client of the widget to the memBus which will arbitrate the DRAM channel
  memBuses.map { mbus =>
    //mbus.fromCoherenceManager(name = Some(portName)){ tsiHostWidget.externalClientNode }
    mbus.from("target-memory-system") { mbus.inwardNode := tsiHostWidget.externalClientNode }
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
  val tsiHostIO = IO(new TSIHostWidgetIO(p(PeripheryTSIHostKey).serialIfWidth)(p))

  // connect to inner modules to the outside (punch through to top)
  tsiHostIO.serial <> outer.tsiHostWidget.module.io.serial

  def tieOffTSIHost() = {
    tsiHostIO.serial.in.bits := 0.U
    tsiHostIO.serial.in.valid := 0.U
    tsiHostIO.serial.out.ready := 0.U
  }
}

/**
 * Trait to create a fake target memory network
 *
 * Used for integration test
 */
trait HasFakeTargetMemoryNetwork extends { this: BaseSubsystem =>
  private val portName = "fake-target-mem-network"

  /* CONNECT WITH SYSBUS/MEMBUS */
  val targetSBus = LazyModule(p(BuildSystemBus)(p))

  private val targetMBusParams = p(MemoryBusKey)
  private val targetL2Params = p(BankedL2Key)

  val MemoryBusParams(targetMemBusBeatBytes, targetMemBusBlockBytes) = targetMBusParams
  val BankedL2Params(targetnMemoryChannels, targetnBanksPerChannel, targetCoherenceManager) = targetL2Params
  val targetnBanks = targetL2Params.nBanks
  val targetCacheBlockBytes = targetMemBusBlockBytes

  private val (target_in, target_out, target_halt) = {
    val targetBH = LazyModule(new TLBroadcast(targetMemBusBlockBytes, 1, p(BroadcastKey).bufferless))
    val targetWW = LazyModule(new TLWidthWidget(targetSBus.beatBytes))
    targetWW.node :*= targetBH.node
    (targetBH.node, targetWW.node, () => None)
  }
  def targetMemBusCanCauseHalt: () => Option[Bool] = target_halt


  require (isPow2(targetnMemoryChannels) || targetnMemoryChannels == 0)
  require (isPow2(targetnBanksPerChannel))
  require (isPow2(targetMemBusBlockBytes))

  private val targetMask = ~BigInt((targetnBanks-1) * targetMemBusBlockBytes)
  val targetMemBuses = Seq.tabulate(targetnMemoryChannels) { channel =>
    val targetMBus = LazyModule(new MemoryBus(targetMBusParams)(p))
    for (bank <- 0 until targetnBanksPerChannel) {
      val offset = (bank * targetnMemoryChannels) + channel
      // connect the sbus to the mbus through the coherenceManager
      ForceFanout(a = true) { implicit p => targetSBus.toMemoryBus { target_in } }
      targetMBus.fromCoherenceManager(None) { TLFilter(TLFilter.mSelectIntersect(AddressSet(offset * targetMemBusBlockBytes, targetMask))) } := target_out
    }
    targetMBus
  }

  val targetSerdes = LazyModule(new TLSerdesser(
        w = p(PeripheryTSIHostKey).serialIfWidth,
        clientParams = p(PeripheryTSIHostKey).serdesParams.clientParams.copy(
          name = "tl-tsi-target-serdes",
          sourceId = IdRange(0, 1)
        ),
        managerParams = p(PeripheryTSIHostKey).serdesParams.managerParams,
        beatBytes = targetSBus.beatBytes,
        onTarget = true))

  // connect serdes client to the target sbus
  targetSBus.fromPort(Some(portName))() := targetSerdes.clientNode

  // connect the mbus to the manager of the serdes
  targetMemBuses.map { m =>
    m.toDRAMController(Some(portName)) { targetSerdes.managerNode }
  }

  /* CONNECT WITHOUT SYSBUS/MEMBUS */
  //val targetSerdes = LazyModule(new TLSerdesser(
  //      w = p(PeripheryTSIHostKey).serialIfWidth,
  //      clientParams = p(PeripheryTSIHostKey).serdesParams.clientParams,
  //      managerParams = p(PeripheryTSIHostKey).serdesParams.managerParams,
  //      beatBytes = sbus.beatBytes,
  //      onTarget = false))

  //targetSerdes.managerNode := TLBuffer() := targetSerdes.clientNode
}

/**
 * Trait to create a top-level IO that connects the fake memory network of a target
 * to the outside world
 *
 * Used for integration test
 */
trait HasFakeTargetMemoryNetworkImp extends LazyModuleImp {
  val outer: HasFakeTargetMemoryNetwork
  implicit val p: Parameters

  // i/o to the outside world
  val fakeTargetIO = IO(new SerialIO(p(PeripheryTSIHostKey).serialIfWidth))

  fakeTargetIO <> outer.targetSerdes.module.io.ser
}
