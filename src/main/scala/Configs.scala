package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.tilelink._
import sifive.blocks.devices.uart._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy.{AsynchronousCrossing, ClockCrossingType, AddressSet}
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
class WithSerialTL(params: Seq[SerialTLParams] = Seq(SerialTLParams())) extends Config((site, here, up) => {
  case SerialTLKey => params
})

class WithSerialTLWidth(width: Int) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(k => k.copy(width=width))
})

class WithSerialTLMasterLocation(masterWhere: TLBusWrapperLocation) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(
    client=s.client.map(_.copy(masterWhere=masterWhere))
  ))
})

class WithSerialTLSlaveLocation(slaveWhere: TLBusWrapperLocation) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(
    manager=s.manager.map(_.copy(slaveWhere=slaveWhere))
  ))
})

class WithSerialTLPBusManager extends WithSerialTLSlaveLocation(PBUS)

class WithSerialTLClient extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(client=Some(SerialTLClientParams())))
})

class WithNoSerialTLClient extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(client=None))
})

class WithSerialTLMem(
  base: BigInt = BigInt("80000000", 16),
  size: BigInt = BigInt("10000000", 16),
  idBits: Int = 8,
  isMainMemory: Boolean = true
) extends Config((site, here, up) => {
  case SerialTLKey => {
    val memParams = ManagerRAMParams(
      address = base,
      size = size
    )
    up(SerialTLKey, site).map { k => k.copy(
      manager = Some(k.manager.getOrElse(SerialTLManagerParams()).copy(
        memParams = Seq(memParams),
        isMemoryDevice = isMainMemory,
        idBits = idBits
      ))
    )}
  }
})

class WithSerialTLBundleParams(params: TLBundleParameters) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(_.copy(bundleParams=params))
})

class WithSerialTLBCE extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(
    bundleParams=s.bundleParams.copy(hasBCE=true)
  ))
})

class WithSerialTLROM(base: BigInt = 0x20000, size: Int = 0x10000) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    manager = k.manager.map { s => s.copy(
      romParams = Seq(ManagerROMParams(address = base, size = size))
    )}
  )}
})

class WithSerialTLROMFile(file: String) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    manager = k.manager.map { s => s.copy(
      romParams = s.romParams.map(_.copy(contentFileName = Some(file)))
    )}
  )}
})

class WithSerialTLCoherentMem(base: BigInt, size: BigInt) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map { k => k.copy(
    manager = Some(k.manager.getOrElse(SerialTLManagerParams())).map(s => s.copy(
      cohParams = s.cohParams :+ ManagerCOHParams(base, size)
    ))
  )}
})

class WithSerialTLClientIdBits(bits: Int) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map { k => k.copy(
    client=k.client.map(_.copy(idBits=bits))
  )}
})

class WithSerialTLClockDirection(provideClockFreqMHz: Option[Int] = None) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(_.copy(provideClockFreqMHz = provideClockFreqMHz))
})

class WithNoSerialTL extends Config((site, here, up) => {
  case SerialTLKey => None
})

class WithTilesStartInReset(harts: Int*) extends Config((site, here, up) => {
  case TileResetCtrlKey => up(TileResetCtrlKey, site).copy(initResetHarts = up(TileResetCtrlKey, site).initResetHarts ++ harts)
})

class WithBootAddrReg(params: BootAddrRegParams = BootAddrRegParams()) extends Config((site, here, up) => {
  case BootAddrRegKey => Some(params)
})

class WithNoBootAddrReg extends Config((site, here, up) => {
  case BootAddrRegKey => None
})

class WithCustomBootPin(params: CustomBootPinParams = CustomBootPinParams()) extends Config((site, here, up) => {
  case CustomBootPinKey => Some(params)
})

class WithCustomBootPinAltAddr(address: BigInt) extends Config((site, here, up) => {
  case CustomBootPinKey => up(CustomBootPinKey, site).map(p => p.copy(customBootAddress = address))
})

class WithNoCustomBootPin extends Config((site, here, up) => {
  case CustomBootPinKey => None
})

class WithScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), banks: Int = 1, partitions: Int = 1, busWhere: TLBusWrapperLocation = SBUS) extends Config((site, here, up) => {
  case BankedScratchpadKey => up(BankedScratchpadKey) ++ (0 until partitions).map { pa => BankedScratchpadParams(
    base + pa * (size / partitions), size / partitions, busWhere = busWhere, name = s"${busWhere.name}-scratchpad", banks = banks) }
})

class WithMbusScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), banks: Int = 1, partitions: Int = 1) extends
    WithScratchpad(base, size, banks, partitions, MBUS)

class WithSbusScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), banks: Int = 1, partitions: Int = 1) extends
    WithScratchpad(base, size, banks, partitions, SBUS)

class WithNoScratchpadMonitors extends Config((site, here, up) => {
  case BankedScratchpadKey => up(BankedScratchpadKey).map(_.copy(disableMonitors=true))
})


class WithUARTTSIClient(initBaudRate: BigInt = BigInt(115200)) extends Config((site, here, up) => {
  case UARTTSIClientKey => Some(UARTTSIClientParams(UARTParams(0, initBaudRate=initBaudRate)))
})

class WithOffchipBus extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) => up(TLNetworkTopologyLocated(InSubsystem)) :+
    OffchipBusTopologyParams(site(OffchipBusKey))
  case OffchipBusKey => up(OffchipBusKey).copy(beatBytes = 8, blockBytes = site(CacheBlockBytes))
})

class WithOffchipBusClient(
  location: TLBusWrapperLocation,
  blockRange: Seq[AddressSet] = Nil,
  replicationBase: Option[BigInt] = None) extends Config((site, here, up) => {
    case TLNetworkTopologyLocated(InSubsystem) => up(TLNetworkTopologyLocated(InSubsystem)) :+
      OffchipBusTopologyConnectionParams(location, blockRange, replicationBase)
})

