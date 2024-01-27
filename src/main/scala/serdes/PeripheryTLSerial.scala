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
  idBits: Int = 8,
  slaveWhere: TLBusWrapperLocation = OBUS
)

// Parameters for a TL client which may probe this system over serial-TL
case class SerialTLClientParams(
  idBits: Int = 8,
  masterWhere: TLBusWrapperLocation = FBUS,
  supportsProbe: Boolean = false
)

// The SerialTL can be configured to be bidirectional if serialTLManagerParams is set
case class SerialTLParams(
  client: Option[SerialTLClientParams] = None,
  manager: Option[SerialTLManagerParams] = None,
  phyParams: SerialPhyParams = ExternalSyncSerialPhyParams(),
  bundleParams: TLBundleParameters = TLSerdesser.STANDARD_TLBUNDLE_PARAMS)

case object SerialTLKey extends Field[Seq[SerialTLParams]](Nil)

trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val (serdessers, serial_tls, serial_tl_debugs) = p(SerialTLKey).zipWithIndex.map { case (params, sid) =>

    val name = s"serial_tl_$sid"
    lazy val manager_bus = params.manager.map(m => locateTLBusWrapper(m.slaveWhere))
    lazy val client_bus = params.client.map(c => locateTLBusWrapper(c.masterWhere))
    val clientPortParams = params.client.map { c => TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = name,
        sourceId = IdRange(0, 1 << c.idBits),
        supportsProbe = if (c.supportsProbe) TransferSizes(client_bus.get.blockBytes, client_bus.get.blockBytes) else TransferSizes.none
      ))
    ) }

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
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsAcquireT   = TransferSizes(1, blockBytes),
          supportsAcquireB   = TransferSizes(1, blockBytes),
          supportsGet        = TransferSizes(1, blockBytes),
          supportsPutFull    = TransferSizes(1, blockBytes),
          supportsPutPartial = TransferSizes(1, blockBytes)
        )},
        beatBytes = manager_bus.get.beatBytes,
        endSinkId = if (cohParams.isEmpty) 0 else (1 << m.idBits),
        minLatency = 1
      )
    }

    val serial_tl_domain = LazyModule(new ClockSinkDomain(name=Some(name)))
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
      bundleParams = params.bundleParams
    )) }
    serdesser.managerNode.foreach { managerNode =>
      manager_bus.get.coupleTo(s"port_named_${name}_out") {
        managerNode := TLSourceShrinker(1 << params.manager.get.idBits) := TLWidthWidget(manager_bus.get.beatBytes) := _
      }
    }
    serdesser.clientNode.foreach { clientNode =>
      client_bus.get.coupleFrom(s"port_named_${name}_in") { _ := TLBuffer() := clientNode }
    }


    // If we provide a clock, generate a clock domain for the outgoing clock
    val serial_tl_clock_freqMHz = params.phyParams match {
      case params: InternalSyncSerialPhyParams => Some(params.freqMHz)
      case params: ExternalSyncSerialPhyParams => None
      case params: SourceSyncSerialPhyParams => Some(params.freqMHz)
    }
    val serial_tl_clock_node = serial_tl_clock_freqMHz.map { f =>
      serial_tl_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(f))))) }
    }
    serial_tl_clock_node.foreach(_ := ClockGroup()(p, ValName(s"${name}_clock")) := allClockGroupsNode)

    val inner_io = serial_tl_domain { InModuleBody {
      val inner_io = IO(params.phyParams.genIO).suggestName(name)

      inner_io match {
        case io: InternalSyncPhitIO => {
          // Outer clock comes from the clock node. Synchronize the serdesser's reset to that
          // clock to get the outer reset
          val outer_clock = serial_tl_clock_node.get.in.head._1.clock
          io.clock_out := outer_clock
          val crossing = Module(new DecoupledSerialPhy(params.phyParams))
          crossing.io.outer_clock := outer_clock
          crossing.io.outer_reset := ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          crossing.io.inner_clock := serdesser.module.clock
          crossing.io.inner_reset := serdesser.module.reset
          crossing.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(io.phitWidth))
          crossing.io.inner_ser <> serdesser.module.io.ser
        }
        case io: ExternalSyncPhitIO => {
          // Outer clock comes from the IO. Synchronize the serdesser's reset to that
          // clock to get the outer reset
          val outer_clock = io.clock_in
          val outer_reset = ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          val crossing = Module(new DecoupledSerialPhy(params.phyParams))
          crossing.io.outer_clock := outer_clock
          crossing.io.outer_reset := ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
          crossing.io.inner_clock := serdesser.module.clock
          crossing.io.inner_reset := serdesser.module.reset
          crossing.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(params.phyParams.phitWidth))
          crossing.io.inner_ser <> serdesser.module.io.ser
        }
        case io: SourceSyncPhitIO => {
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
          val crossing = Module(new CreditedSerialPhy(params.phyParams))
          crossing.io.incoming_clock := incoming_clock
          crossing.io.incoming_reset := incoming_reset
          crossing.io.outgoing_clock := outgoing_clock
          crossing.io.outgoing_reset := outgoing_reset
          crossing.io.inner_clock := serdesser.module.clock
          crossing.io.inner_reset := serdesser.module.reset
          crossing.io.inner_ser <> serdesser.module.io.ser

          crossing.io.outer_ser <> io.viewAsSupertype(new CreditedPhitIO(params.phyParams.phitWidth))
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
