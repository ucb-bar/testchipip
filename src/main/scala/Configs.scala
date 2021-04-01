package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy.{AsynchronousCrossing, ClockCrossingType}
import freechips.rocketchip.unittest.UnitTests

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
  case SerialTLKey => Some(SerialTLParams(
    memParams = MasterPortParams(
      base = BigInt("10000000", 16),
      size = BigInt("00001000", 16),
      beatBytes = site(MemoryBusKey).beatBytes,
      idBits = 4
    ),
    width = 4
  ))
})

class WithSerialTLWidth(width: Int) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(k => k.copy(width=width))
})

class WithAXIMemOverSerialTL(axiMemOverSerialTLParams: AXIMemOverSerialTLClockParams) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(k => k.copy(axiMemOverSerialTLParams=Some(axiMemOverSerialTLParams)))
})

class WithSerialTLAsyncResetQueue extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(k => k.copy(asyncResetQueue = true))
})

class WithSerialPBusMem extends Config((site, here, up) => {
  case SerialTLAttachKey => up(SerialTLAttachKey, site).copy(slaveWhere = PBUS)
})

class WithSerialSlaveCrossingType(xType: ClockCrossingType) extends Config((site, here, up) => {
  case SerialTLAttachKey => up(SerialTLAttachKey, site).copy(slaveCrossingType = xType)
})

class WithAsynchronousSerialSlaveCrossing extends WithSerialSlaveCrossingType(AsynchronousCrossing())

class WithSerialTLMem(
  base: BigInt = BigInt("80000000", 16),
  size: BigInt = BigInt("10000000", 16),
  isMainMemory: Boolean = true
) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    memParams = k.memParams.copy(
      base = base,
      size = size,
    ),
    isMemoryDevice = isMainMemory
  )}
})


class WithSerialTLROMFile(file: String) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    romParams = k.romParams.copy(contentFileName = Some(file))
  ) }
})

class WithTilesStartInReset(harts: Int*) extends Config((site, here, up) => {
  case TileResetCtrlKey => up(TileResetCtrlKey, site).copy(initResetHarts = up(TileResetCtrlKey, site).initResetHarts ++ harts)
})
