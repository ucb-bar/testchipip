package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.tilelink._
import sifive.blocks.devices.uart._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.unittest.UnitTests
import freechips.rocketchip.util.{ClockGateImpl}

// Deprecated: use Constellation's network-on-chip generators instead of this
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

class WithTestChipEICGWrapper extends Config((site, here, up) => {
   case ClockGateImpl => () => new testchipip.EICG_wrapper
})

//----------------------
// Block Device Configs
//----------------------

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

//--------------------------
// Serial Tilelink Configs
//--------------------------

// Attach a sequence of serial-TL ports to a system
class WithSerialTL(params: Seq[SerialTLParams] = Seq(SerialTLParams())) extends Config((site, here, up) => {
  case SerialTLKey => params
})

// Modify the width of all attached serial-TL ports
class WithSerialTLWidth(width: Int) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(k => k.copy(width=width))
})

// Set the bus which the serial-TL client will master on this system for all attached serial-TL ports
class WithSerialTLMasterLocation(masterWhere: TLBusWrapperLocation) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(
    client=s.client.map(_.copy(masterWhere=masterWhere))
  ))
})

// Set the bus which the serial-TL manager will attach to on this system for all attached serial-TL ports
class WithSerialTLSlaveLocation(slaveWhere: TLBusWrapperLocation) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(
    manager=s.manager.map(_.copy(slaveWhere=slaveWhere))
  ))
})

class WithSerialTLPBusManager extends WithSerialTLSlaveLocation(PBUS)

// Add a client interface to all attached serial-TL ports on this system
class WithSerialTLClient extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(client=Some(SerialTLClientParams())))
})

// Remove the client interface from all attached serial-TL ports on this system
class WithNoSerialTLClient extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(client=None))
})

// Specify a read/write memory region to all attached serial-TL ports on this system
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

// Specify the TL merged bundle parameters for all attached serial-TL ports on this system
class WithSerialTLBundleParams(params: TLBundleParameters) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(_.copy(bundleParams=params))
})

// Enable the TL-C protoocl for all attached serial-TL ports on this system
class WithSerialTLBCE extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(s => s.copy(
    bundleParams=s.bundleParams.copy(hasBCE=true)
  ))
})

// Attach a read-only-memory to all serial-TL ports on this system
class WithSerialTLROM(base: BigInt = 0x20000, size: Int = 0x10000) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    manager = k.manager.map { s => s.copy(
      romParams = Seq(ManagerROMParams(address = base, size = size))
    )}
  )}
})

// Specify the ROM contents for any read-only-memories attached to serial-TL ports on this system
// Note: This only affects simulation
class WithSerialTLROMFile(file: String) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey, site).map { k => k.copy(
    manager = k.manager.map { s => s.copy(
      romParams = s.romParams.map(_.copy(contentFileName = Some(file)))
    )}
  )}
})

// Attach a coherent read/write/cacheable memory to all serial-TL ports on this system
class WithSerialTLCoherentMem(base: BigInt, size: BigInt) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map { k => k.copy(
    manager = Some(k.manager.getOrElse(SerialTLManagerParams())).map(s => s.copy(
      cohParams = s.cohParams :+ ManagerCOHParams(base, size)
    ))
  )}
})

// Specify the number of client ID bits for serial-TL ports on this system which master this system
class WithSerialTLClientIdBits(bits: Int) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map { k => k.copy(
    client=k.client.map(_.copy(idBits=bits))
  )}
})

// Specify the clock direction for all source/sink-synchronous serial-TL interfaces in this system
class WithSerialTLClockDirection(provideClockFreqMHz: Option[Int] = None) extends Config((site, here, up) => {
  case SerialTLKey => up(SerialTLKey).map(_.copy(provideClockFreqMHz = provideClockFreqMHz))
})

// Remove all serial-TL ports from this system
class WithNoSerialTL extends Config((site, here, up) => {
  case SerialTLKey => Nil
})

//---------------------------
// Bringup Configs
//---------------------------

// Specify which Tiles will stay in reset, controlled by the TileResetCtrl block
class WithTilesStartInReset(harts: Int*) extends Config((site, here, up) => {
  case TileResetCtrlKey => up(TileResetCtrlKey, site).copy(initResetHarts = up(TileResetCtrlKey, site).initResetHarts ++ harts)
})

// Specify the parameters for the BootAddrReg
class WithBootAddrReg(params: BootAddrRegParams = BootAddrRegParams()) extends Config((site, here, up) => {
  case BootAddrRegKey => Some(params)
})

// Remove the BootAddrReg from the syste. This will likely break the default bootrom
class WithNoBootAddrReg extends Config((site, here, up) => {
  case BootAddrRegKey => None
})

// Attach a boot-select pin to the system with given parameters
class WithCustomBootPin(params: CustomBootPinParams = CustomBootPinParams()) extends Config((site, here, up) => {
  case CustomBootPinKey => Some(params)
})

// Specify the alternate boot addres the custom boot pin will select
class WithCustomBootPinAltAddr(address: BigInt) extends Config((site, here, up) => {
  case CustomBootPinKey => up(CustomBootPinKey, site).map(p => p.copy(customBootAddress = address))
})

// Remove the boot-select pin from the system
class WithNoCustomBootPin extends Config((site, here, up) => {
  case CustomBootPinKey => None
})

// Attach a TSI-over-UART-to-TileLink device to this system
class WithUARTTSIClient(initBaudRate: BigInt = BigInt(115200)) extends Config((site, here, up) => {
  case UARTTSIClientKey => Some(UARTTSIClientParams(UARTParams(0, initBaudRate=initBaudRate)))
})

//-------------------------
// Scratchpad Configs
//-------------------------

// Attach a non-cacheable read/write general-purpose SRAM-backed bankable memory to sum bus in the system
class WithScratchpad(
  base: BigInt = 0x80000000L,
  size: BigInt = (4 << 20),
  banks: Int = 1,
  partitions: Int = 1,
  busWhere: TLBusWrapperLocation = SBUS,
  subBanks: Int = 1,
  buffer: BufferParams = BufferParams.none,
  outerBuffer: BufferParams = BufferParams.none
) extends Config((site, here, up) => {
  case BankedScratchpadKey => up(BankedScratchpadKey) ++ (0 until partitions).map { pa => BankedScratchpadParams(
    base + pa * (size / partitions),
    size / partitions,
    busWhere = busWhere,
    name = s"${busWhere.name}-scratchpad",
    banks = banks,
    buffer = buffer,
    outerBuffer = outerBuffer,
    subBanks = subBanks
  )}
})

class WithMbusScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), banks: Int = 1, partitions: Int = 1, subBanks: Int = 1) extends
    WithScratchpad(base, size, banks, partitions, MBUS, subBanks)

class WithSbusScratchpad(base: BigInt = 0x80000000L, size: BigInt = (4 << 20), banks: Int = 1, partitions: Int = 1, subBanks: Int = 1) extends
    WithScratchpad(base, size, banks, partitions, SBUS, subBanks)

// Remove TL monitors from the Scratchpads. This is used to enable deduplication for VLSI flows
class WithNoScratchpadMonitors extends Config((site, here, up) => {
  case BankedScratchpadKey => up(BankedScratchpadKey).map(_.copy(disableMonitors=true))
})

//-------------------------
// OffchipBus Configs
//-------------------------

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
