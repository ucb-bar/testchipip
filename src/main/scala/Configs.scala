package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy.{AsynchronousCrossing, ClockCrossingType, AddressSet}
import freechips.rocketchip.unittest.UnitTests
import sifive.blocks.devices.uart.{UARTParams}

class WithRingSystemBus(
    buffer: TLNetworkBufferParams = TLNetworkBufferParams.default)
    extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) =>
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo =>
      topo match {
        case j: JustOneBusTopologyParams =>
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: SystemBusParams) => (SBUS, RingSystemBusParams(sbus_params, buffer))
            case a => a
          }
        ), j.connections)
        case x => x
      }
    )
})

class WithTestChipUnitTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) =>
    TestChipUnitTests(testParams)
})

class WithClockUtilTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) => ClockUtilTests()
})

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

class ClockUtilTestConfig extends Config(
  new WithClockUtilTests ++ new BaseConfig)

class WithBlockDevice(enable: Boolean = true) extends Config((site, here, up) => {
  case BlockDeviceKey => enable match {
    case true => Some(BlockDeviceConfig())
    case false => None
  }
})

class WithBlockDeviceLocations(slaveWhere: TLBusWrapperLocation = PBUS, masterWhere: TLBusWrapperLocation = FBUS) extends Config((site, here, up) => {
  case BlockDeviceAttachKey => BlockDeviceAttachParams(slaveWhere, masterWhere)
})

class WithNBlockDeviceTrackers(n: Int) extends Config((site, here, up) => {
  case BlockDeviceKey => up(BlockDeviceKey, site) match {
    case Some(a) => Some(a.copy(nTrackers = n))
    case None => None
  }
})

// Default size should be tiny
class WithDefaultSerialTL extends Config((site, here, up) => {
  case SerialTLKey => Some(SerialTLParams())
})

class WithSerialTLWidth(width: Int) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(k => k.copy(width=width))
})

class WithAXIMemOverSerialTL(axiMemOverSerialTLParams: AXIMemOverSerialTLClockParams) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(serialManagerParams=s.serialManagerParams.map(
    _.copy(axiMemOverSerialTLParams=Some(axiMemOverSerialTLParams)))))
})

class WithSerialSlaveCrossingType(xType: ClockCrossingType) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(attachParams=s.attachParams.copy(slaveCrossingType = xType)))
})

class WithAsynchronousSerialSlaveCrossing extends WithSerialSlaveCrossingType(AsynchronousCrossing())

class WithSerialTLMem(
  base: BigInt = BigInt("80000000", 16),
  size: BigInt = BigInt("10000000", 16),
  idBits: Int = 8,
  isMainMemory: Boolean = true
) extends Config((site, here, up) => {
  case SerialTLKey => {
    val masterPortParams = MasterPortParams(
      base = base,
      size = size,
      idBits = idBits,
      beatBytes = site(MemoryBusKey).beatBytes
    )
    up(SerialTLKey, site).map { k => k.copy(
      serialManagerParams = Some(k.serialManagerParams.getOrElse(SerialTLManagerParams(memParams = masterPortParams))
        .copy(memParams = masterPortParams, isMemoryDevice = isMainMemory)
      )
    )}
  }
})

class WithSerialTLROM extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    serialManagerParams = k.serialManagerParams.map { s => s.copy(
      romParams = Some(SerialTLROMParams())
    )}
  )}
})

class WithSerialTLROMFile(file: String) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    serialManagerParams = k.serialManagerParams.map { s => s.copy(
      romParams = s.romParams.map(_.copy(contentFileName = Some(file)))
    )}
  )}
})

class WithTilesStartInReset(harts: Int*) extends Config((site, here, up) => {
  case TileResetCtrlKey => up(TileResetCtrlKey, site).copy(initResetHarts = up(TileResetCtrlKey, site).initResetHarts ++ harts)
})

class WithNoSerialTL extends Config((site, here, up) => {
  case SerialTLKey => None
})

class WithUARTTSITLClient(initBaudRate: BigInt = BigInt(115200)) extends Config((site, here, up) => {
  case UARTTSITLClientKey => Some(UARTTSITLClientParams(UARTParams(0, initBaudRate=initBaudRate)))
})

class WithSerialTLClockDirection(provideClock: Boolean = false) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(_.copy(provideClock = provideClock))
})

class WithOffchipBus extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => up(TLNetworkTopologyLocated(InSubsystem)) :+
    OffchipBusTopologyParams(SystemBusParams(beatBytes = 8, blockBytes = site(CacheBlockBytes)))
})

class WithOffchipBusManager(
  location: TLBusWrapperLocation,
  blockRange: Seq[AddressSet] = Nil,
  replicationBase: Option[BigInt] = None) extends Config((site, here, up) => {
    case TLNetworkTopologyLocated(InSubsystem) => up(TLNetworkTopologyLocated(InSubsystem)) :+
      OffchipBusTopologyConnectionParams(location, blockRange, replicationBase)
})

