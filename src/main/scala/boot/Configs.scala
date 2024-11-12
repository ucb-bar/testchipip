package testchipip.boot

import chisel3._
import org.chipsalliance.cde.config.{Parameters, Config}
import sifive.blocks.devices.uart.{UARTParams}

//---------------------------
// Bringup/Boot Configs
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

// Specify the alternate boot addresses the custom boot pin will select
class WithCustomBootPinAltAddrs(addresses: BigInt*) extends Config((site, here, up) => {
  case CustomBootPinKey => up(CustomBootPinKey, site).map(p => p.copy(customBootAddresses = addresses))
})

// Remove the boot-select pin from the system
class WithNoCustomBootPin extends Config((site, here, up) => {
  case CustomBootPinKey => None
})

