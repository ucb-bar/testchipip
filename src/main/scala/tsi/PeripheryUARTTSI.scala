package testchipip.tsi

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}

import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{AsyncQueue, ResetCatchAndSync}

import sifive.blocks.devices.uart._
import testchipip.serdes._
import testchipip.uart.{UARTToSerial}


case class UARTTSIClientParams(
  uartParams: UARTParams = UARTParams(0),
  tlbus: TLBusWrapperLocation = FBUS,
  uartClockFreqHz: Option[BigInt] = None
)

case object UARTTSIClientKey extends Field[Option[UARTTSIClientParams]](None)

class UARTTSIIO(val uartParams: UARTParams) extends Bundle {
  val uart = new UARTPortIO(uartParams)
  val dropped = Output(Bool())
  val tsi2tl_state = Output(UInt())
}

class UARTTSIClockedIO(uartParams: UARTParams) extends UARTTSIIO(uartParams) with HasClockIn

// This trait adds a UART port to the subsystem that transports TSI.
// It is supposed to be used for FPGA-harnesses or FPGA prototypes
// This should not be used for ASIC implemnetations
trait CanHavePeripheryUARTTSI { this: BaseSubsystem =>
  val uart_tsi = p(UARTTSIClientKey).map { params =>
    val tlbus = locateTLBusWrapper(params.tlbus)
    val uartParams = params.uartParams
    val tsi2tl = tlbus { LazyModule(new TSIToTileLink) }
    tlbus.coupleFrom("uart_tsi") { _ := tsi2tl.node }
    val uart_bus_io = tlbus { InModuleBody {
      val width_adapter = Module(new SerialWidthAdapter(8, TSI.WIDTH))
      tsi2tl.module.io.tsi.flipConnect(width_adapter.io.wide)

      params.uartClockFreqHz match {
        case Some(uartClockFreqHz) =>
          val uart_tsi_io = IO(new UARTTSIClockedIO(uartParams))
          val uart_reset = ResetCatchAndSync(uart_tsi_io.clock_in, tsi2tl.module.reset.asBool)
          val uart_to_serial = withClockAndReset(uart_tsi_io.clock_in, uart_reset) {
            Module(new UARTToSerial(uartClockFreqHz, uartParams))
          }

          val uart_to_tsi = Module(new AsyncQueue(UInt(8.W)))
          uart_to_tsi.io.enq_clock := uart_tsi_io.clock_in
          uart_to_tsi.io.enq_reset := uart_reset
          uart_to_tsi.io.deq_clock := tsi2tl.module.clock
          uart_to_tsi.io.deq_reset := tsi2tl.module.reset
          uart_to_tsi.io.enq <> uart_to_serial.io.serial.out
          width_adapter.io.narrow.in <> uart_to_tsi.io.deq

          val tsi_to_uart = Module(new AsyncQueue(UInt(8.W)))
          tsi_to_uart.io.enq_clock := tsi2tl.module.clock
          tsi_to_uart.io.enq_reset := tsi2tl.module.reset
          tsi_to_uart.io.deq_clock := uart_tsi_io.clock_in
          tsi_to_uart.io.deq_reset := uart_reset
          tsi_to_uart.io.enq <> width_adapter.io.narrow.out
          uart_to_serial.io.serial.in <> tsi_to_uart.io.deq

          uart_tsi_io.uart <> uart_to_serial.io.uart
          uart_tsi_io.dropped := uart_to_serial.io.dropped
          uart_tsi_io.tsi2tl_state := tsi2tl.module.io.state
          uart_tsi_io

        case None =>
          val uart_to_serial = Module(new UARTToSerial(tlbus.dtsFrequency.get, uartParams))
          width_adapter.io.narrow.flipConnect(uart_to_serial.io.serial)
          val uart_tsi_io = IO(new UARTTSIIO(uartParams))
          uart_tsi_io.uart <> uart_to_serial.io.uart
          uart_tsi_io.dropped := uart_to_serial.io.dropped
          uart_tsi_io.tsi2tl_state := tsi2tl.module.io.state
          uart_tsi_io
      }
    } }

    val uart_tsi_io = InModuleBody {
      val uart_tsi_io = params.uartClockFreqHz match {
        case Some(_) => IO(new UARTTSIClockedIO(uartParams))
        case None => IO(new UARTTSIIO(uartParams))
      }
      uart_tsi_io <> uart_bus_io
      uart_tsi_io
    }
    uart_tsi_io
  }
}
