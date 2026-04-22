package testchipip.serdes

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import testchipip.util.{ClockedIO}
import testchipip.soc.{OBUS}
import testchipip.clocking.{TLSerialClockDivider}

// Parameters for a read-only-memory that appears over serial-TL
case class ManagerROMParams(
  address: BigInt = 0x20000,
  size: Int = 0x10000,
  contentFileName: Option[String] = None) // If unset, generates a JALR to DRAM_BASE

// Parameters for a read/write memory that appears over serial-TL
case class ManagerRAMParams(
  address: BigInt,
  size: BigInt)

// Parameters for a coherent cacheable read/write memory that appears over serial-TL
case class ManagerCOHParams(
  address: BigInt,
  size: BigInt)

// Parameters for a set of memory regions that appear over serial-TL
case class SerialTLManagerParams(
  memParams: Seq[ManagerRAMParams] = Nil,
  romParams: Seq[ManagerROMParams] = Nil,
  cohParams: Seq[ManagerCOHParams] = Nil,
  isMemoryDevice: Boolean = false,
  sinkIdBits: Int = 8,
  totalIdBits: Int = 8,
  cacheIdBits: Int = 2,
  slaveWhere: TLBusWrapperLocation = OBUS
)

// Parameters for a TL client which may probe this system over serial-TL
case class SerialTLClientParams(
  totalIdBits: Int = 8,
  cacheIdBits: Int = 2,
  masterWhere: TLBusWrapperLocation = FBUS,
  supportsProbe: Boolean = false
)

// Parameters for an optional TL memory-mapped clock divider inside a serial TL clock domain.
// address:  base address of the 4 KiB MMIO register region exposed on the bus.
// busWhere: which TL bus hosts the MMIO register (default CBUS — always reachable from UART-TSI).
// divBits:  width of the divisor register (default 8 → divide by up to 255).
// enable:   when false the divider is wired as a pass-through (RTL simulation only).
case class SerialTLClockDividerParams(
  address:  BigInt,
  busWhere: TLBusWrapperLocation = CBUS,
  divBits:  Int     = 8,
  enable:   Boolean = true)

// The SerialTL can be configured to be bidirectional if serialTLManagerParams is set
case class SerialTLParams(
  client: Option[SerialTLClientParams] = None,
  manager: Option[SerialTLManagerParams] = None,
  phyParams: SerialPhyParams = DecoupledExternalSyncSerialPhyParams(),
  bundleParams: TLBundleParameters = TLSerdesser.STANDARD_TLBUNDLE_PARAMS,
  // When set, inserts a TLSerialClockDivider into the serial TL clock domain so the
  // serial_tl clock can be independently controlled at runtime via MMIO.
  clockDividerParams: Option[SerialTLClockDividerParams] = None)

case object SerialTLKey extends Field[Seq[SerialTLParams]](Nil)

trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val tlChannels = 5
  val (serdessers, serial_tls, serial_tl_debugs) = p(SerialTLKey).zipWithIndex.map { case (params, sid) =>

    val name = s"serial_tl_$sid"
    lazy val manager_bus = params.manager.map(m => locateTLBusWrapper(m.slaveWhere))
    lazy val client_bus = params.client.map(c => locateTLBusWrapper(c.masterWhere))
    val clientPortParams = params.client.map { c => TLMasterPortParameters.v1(
      clients = Seq.tabulate(1 << c.cacheIdBits){ i => TLMasterParameters.v1(
        name = s"serial_tl_${sid}_${i}",
        sourceId = IdRange(i << (c.totalIdBits - c.cacheIdBits), (i + 1) << (c.totalIdBits - c.cacheIdBits)),
        supportsProbe = if (c.supportsProbe) TransferSizes(client_bus.get.blockBytes, client_bus.get.blockBytes) else TransferSizes.none
      )}
    )}

    val managerPortParams = params.manager.map { m =>
      val memParams = m.memParams
      val romParams = m.romParams
      val cohParams = m.cohParams
      val memDevice = if (m.isMemoryDevice) new MemoryDevice else new SimpleDevice("lbwif-readwrite", Nil)
      val romDevice = new SimpleDevice("lbwif-readonly", Nil)
      val blockBytes = manager_bus.get.blockBytes
      TLSlavePortParameters.v1(
        managers = memParams.map { memParams => TLSlaveParameters.v1(
          address            = AddressSet.misaligned(memParams.address, memParams.size),
          resources          = memDevice.reg,
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsGet        = TransferSizes(1, blockBytes),
          supportsPutFull    = TransferSizes(1, blockBytes),
          supportsPutPartial = TransferSizes(1, blockBytes)
        )} ++ romParams.map { romParams => TLSlaveParameters.v1(
          address            = List(AddressSet(romParams.address, romParams.size-1)),
          resources          = romDevice.reg,
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsGet        = TransferSizes(1, blockBytes),
          fifoId             = Some(0)
        )} ++ cohParams.map { cohParams => TLSlaveParameters.v1(
          address            = AddressSet.misaligned(cohParams.address, cohParams.size),
          regionType         = RegionType.TRACKED, // cacheable
          executable         = true,
          supportsAcquireT   = TransferSizes(1, blockBytes),
          supportsAcquireB   = TransferSizes(1, blockBytes),
          supportsGet        = TransferSizes(1, blockBytes),
          supportsPutFull    = TransferSizes(1, blockBytes),
          supportsPutPartial = TransferSizes(1, blockBytes)
        )},
        beatBytes = manager_bus.get.beatBytes,
        endSinkId = if (cohParams.isEmpty) 0 else (1 << m.sinkIdBits),
        minLatency = 1
      )
    }

    val serial_tl_domain = LazyModule(new ClockSinkDomain(name=Some(s"SerialTL$sid")))
    serial_tl_domain.clockNode := manager_bus.getOrElse(client_bus.get).fixedClockNode

    if (manager_bus.isDefined) require(manager_bus.get.dtsFrequency.isDefined,
      s"Manager bus ${manager_bus.get.busName} must provide a frequency")
    if (client_bus.isDefined) require(client_bus.get.dtsFrequency.isDefined,
      s"Client bus ${client_bus.get.busName} must provide a frequency")
    if (manager_bus.isDefined && client_bus.isDefined) {
      val managerFreq = manager_bus.get.dtsFrequency.get
      val clientFreq = client_bus.get.dtsFrequency.get
      require(managerFreq == clientFreq, s"Mismatching manager freq $managerFreq != client freq $clientFreq")
    }

    val serdesser = serial_tl_domain { LazyModule(new TLSerdesser(
      flitWidth = params.phyParams.flitWidth,
      clientPortParams = clientPortParams,
      managerPortParams = managerPortParams,
      bundleParams = params.bundleParams,
      nameSuffix = Some(name)
    )) }
    serdesser.managerNode.foreach { managerNode =>
      val maxClients = 1 << params.manager.get.cacheIdBits
      val maxIdsPerClient = 1 << (params.manager.get.totalIdBits - params.manager.get.cacheIdBits)
      manager_bus.get.coupleTo(s"port_named_${name}_out") {
        (managerNode
          := TLProbeBlocker(p(CacheBlockBytes))
          := TLSourceAdjuster(maxClients, maxIdsPerClient)
          := TLSourceCombiner(maxIdsPerClient)
          := TLWidthWidget(manager_bus.get.beatBytes)
          := _)
      }
    }
    serdesser.clientNode.foreach { clientNode =>
      client_bus.get.coupleFrom(s"port_named_${name}_in") { _ := TLBuffer() := clientNode }
    }


    // If we provide a clock, generate a clock domain for the outgoing clock.
    // DecoupledInternalSyncWithClockRefSerialPhyParams exposes clock_ref on the IO bundle
    // so the harness drives it directly; no diplomacy clock sink needed for that case.
    val serial_tl_clock_freqMHz = params.phyParams match {
      case params: DecoupledInternalSyncSerialPhyParams => Some(params.freqMHz)
      case params: DecoupledInternalSyncWithClockRefSerialPhyParams => None
      case params: DecoupledExternalSyncSerialPhyParams => None
      case params: CreditedSourceSyncSerialPhyParams => Some(params.freqMHz.toDouble)
    }
    val serial_tl_clock_node = serial_tl_clock_freqMHz.map { f =>
      serial_tl_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(f))))) }
    }
    params.clockDividerParams match {
      case Some(divParams) =>
        // Insert TLSerialClockDivider between allClockGroupsNode and the serial TL clock
        // sink.  The ClockGroupIdentityNode (divider.clockNode) sits between
        // allClockGroupsNode (ClockGroupImp) and the ClockGroupingNode (ClockGroupImp →
        // ClockImp), keeping type compatibility.  allClockGroupsNode will present this
        // clock member as "serial_tl_N_clock_0" — the same name as without the divider —
        // so WithNexysVideoSerialTLClockFromPLL can intercept it by matching "serial_tl".
        serial_tl_clock_node.foreach { clockSink =>
          val bus = locateTLBusWrapper(divParams.busWhere)
          // Instantiate inside the bus's clock domain so LazyModuleImp has an implicit clock.
          val divider = bus { LazyModule(new TLSerialClockDivider(
            divParams.address, bus.beatBytes, divParams.divBits, divParams.enable)) }
          bus.coupleTo(s"${name}_clk_div") {
            divider.tlNode := TLFragmenter(bus.beatBytes, bus.blockBytes) := _
          }
          clockSink :=
            ClockGroup()(p, ValName(s"${name}_clock")) :=
            divider.clockNode :=
            allClockGroupsNode
        }
      case None =>
        serial_tl_clock_node.foreach(_ := ClockGroup()(p, ValName(s"${name}_clock")) := allClockGroupsNode)
    }

    val inner_io = serial_tl_domain { InModuleBody {
      val inner_io = IO(params.phyParams.genIO).suggestName(name)

      inner_io match {
        case io: DecoupledInternalSyncPhitIOWithClockRef => {
          // Outer clock comes from clock_ref, driven by the harness from a dedicated PLL output.
          val outer_clock = io.clock_ref
          io.clock_out := outer_clock
          val phy = Module(new DecoupledSerialPhy(tlChannels, params.phyParams))
          phy.io.outer_clock := outer_clock
          phy.io.outer_reset := ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          phy.io.inner_clock := serdesser.module.clock
          phy.io.inner_reset := serdesser.module.reset
          phy.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(io.phitWidth))
          phy.io.inner_ser <> serdesser.module.io.ser
        }
        case io: DecoupledInternalSyncPhitIO => {
          // Outer clock comes from the clock node. Synchronize the serdesser's reset to that
          // clock to get the outer reset
          val outer_clock = serial_tl_clock_node.get.in.head._1.clock
          io.clock_out := outer_clock
          val phy = Module(new DecoupledSerialPhy(tlChannels, params.phyParams))
          phy.io.outer_clock := outer_clock
          phy.io.outer_reset := ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          phy.io.inner_clock := serdesser.module.clock
          phy.io.inner_reset := serdesser.module.reset
          phy.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(io.phitWidth))
          phy.io.inner_ser <> serdesser.module.io.ser
        }
        case io: DecoupledExternalSyncPhitIO => {
          // Outer clock comes from the IO. Synchronize the serdesser's reset to that
          // clock to get the outer reset
          val outer_clock = io.clock_in
          val outer_reset = ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          val phy = Module(new DecoupledSerialPhy(tlChannels, params.phyParams))
          phy.io.outer_clock := outer_clock
          phy.io.outer_reset := ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          phy.io.inner_clock := serdesser.module.clock
          phy.io.inner_reset := serdesser.module.reset
          phy.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(params.phyParams.phitWidth))
          phy.io.inner_ser <> serdesser.module.io.ser
        }
        case io: CreditedSourceSyncPhitIO => {
          // 3 clock domains -
          // - serdesser's "Inner clock": synchronizes signals going to the digital logic
          // - outgoing clock: synchronizes signals going out
          // - incoming clock: synchronizes signals coming in
          val outgoing_clock = serial_tl_clock_node.get.in.head._1.clock
          val outgoing_reset = ResetCatchAndSync(outgoing_clock, serdesser.module.reset.asBool)
          val incoming_clock = io.clock_in
          val incoming_reset = ResetCatchAndSync(incoming_clock, io.reset_in.asBool)
          io.clock_out := outgoing_clock
          io.reset_out := outgoing_reset.asAsyncReset
          val phy = Module(new CreditedSerialPhy(tlChannels, params.phyParams))
          phy.io.incoming_clock := incoming_clock
          phy.io.incoming_reset := incoming_reset
          phy.io.outgoing_clock := outgoing_clock
          phy.io.outgoing_reset := outgoing_reset
          phy.io.inner_clock := serdesser.module.clock
          phy.io.inner_reset := serdesser.module.reset
          phy.io.inner_ser <> serdesser.module.io.ser

          phy.io.outer_ser <> io.viewAsSupertype(new ValidPhitIO(params.phyParams.phitWidth))
        }
      }
      inner_io
    }}
    val outer_io = InModuleBody {
      val outer_io = IO(params.phyParams.genIO).suggestName(name)
      outer_io <> inner_io
      outer_io
    }

    val inner_debug_io = serial_tl_domain { InModuleBody {
      val inner_debug_io = IO(new SerdesDebugIO).suggestName(s"${name}_debug")
      inner_debug_io := serdesser.module.io.debug
      inner_debug_io
    }}
    val outer_debug_io = InModuleBody {
      val outer_debug_io = IO(new SerdesDebugIO).suggestName(s"${name}_debug")
      outer_debug_io := inner_debug_io
      outer_debug_io
    }
    (serdesser, outer_io, outer_debug_io)
  }.unzip3
}
