package testchipip.tsi

import chisel3._
import org.chipsalliance.cde.config.{Parameters, Config}
import sifive.blocks.devices.uart.{UARTParams}

// Attach a TSI-over-UART-to-TileLink device to this system
class WithUARTTSIClient(initBaudRate: BigInt = BigInt(115200)) extends Config((site, here, up) => {
  case UARTTSIClientKey => Some(UARTTSIClientParams(UARTParams(0, initBaudRate=initBaudRate)))
})

class WithSerialTSI(harnessClockFreqMHz: Double) extends Config((site, here, up) => {
  case SerialTSIKey => Some(SerialTSIParams(
    testchipip.serdes.SerialTLParams(
      client = Some(testchipip.serdes.SerialTLClientParams(totalIdBits=4)), // serial-tilelink interface will master the FBUS, and support 4 idBits
      phyParams = testchipip.serdes.InternalSyncSerialPhyParams(
        phitWidth=32,
        flitWidth=32,
        freqMHz=harnessClockFreqMHz.toInt)
    )))
})
