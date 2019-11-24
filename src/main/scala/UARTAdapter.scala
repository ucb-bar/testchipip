package testchipip

import chisel3._
import chisel3.util._
import chisel3.core.{IntParam, StringParam}

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.subsystem.{PeripheryBusKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.config.{Field}

import sifive.blocks.devices.uart._

object UARTAdapterConsts {
  val DATA_WIDTH = 8
}
import UARTAdapterConsts._

case object UARTAdapterKey extends Field[Seq[UARTAdapterParams]](Nil)

case class UARTAdapterParams(
  baudRateInit: BigInt = BigInt(115200)
)

class UARTAdapter(uartno: Int)(implicit p: Parameters) extends Module
{
  val io = IO(new Bundle {
    val uart = Flipped(new UARTPortIO)
  })

  val frequency = p(PeripheryBusKey).frequency
  val baudrate = p(UARTAdapterKey)(uartno).baudRateInit
  val div = (p(PeripheryBusKey).frequency / baudrate).toInt

  val txfifo = Module(new Queue(UInt(DATA_WIDTH.W), 128))
  val rxfifo = Module(new Queue(UInt(DATA_WIDTH.W), 128))

  val uart_io = io.uart

  val sTxIdle :: sTxWait :: sTxData :: sTxBreak :: Nil = Enum(4)
  val txState = RegInit(sTxIdle)
  val txData = Reg(UInt(DATA_WIDTH.W))
  // iterate through bits in byte to deserialize
  val (txDataIdx, txDataWrap) = Counter(txState === sTxData, DATA_WIDTH)
  // iterate using div to convert clock rate to baud
  val (txBaudCount, txBaudWrap) = Counter(txState === sTxWait, div)
  val (txSlackCount, txSlackWrap) = Counter(txState === sTxIdle && uart_io.txd === 0.U, 4)

  switch(txState) {
    is(sTxIdle) {
      when(txSlackWrap) {
        txData  := 0.U
        txState := sTxWait
      }
    }
    is(sTxWait) {
      when(txBaudWrap) {
        txState := sTxData
      }
    }
    is(sTxData) {
      txData := txData | (uart_io.txd << txDataIdx)
      when(txDataWrap) {
        txState := Mux(uart_io.txd === 1.U, sTxIdle, sTxBreak)
      }.otherwise {
        txState := sTxWait
      }
    }
    is(sTxBreak) {
      when(uart_io.txd === 1.U) {
        txState := sTxIdle
      }
    }
  }

  txfifo.io.enq.bits  := txData
  txfifo.io.enq.valid := txDataWrap

  val sRxIdle :: sRxStart :: sRxData :: Nil = Enum(3)
  val rxState = RegInit(sRxIdle)
  // iterate using div to convert clock rate to baud
  val (rxBaudCount, rxBaudWrap) = Counter(true.B, div)
  // iterate through bits in byte to deserialize
  val (rxDataIdx, rxDataWrap) = Counter(rxState === sRxData && rxBaudWrap, DATA_WIDTH)

  uart_io.rxd := 1.U
  switch(rxState) {
    is(sRxIdle) {
      uart_io.rxd := 1.U
      when (rxBaudWrap && rxfifo.io.deq.valid) {
        rxState := sRxStart
      }
    }
    is(sRxStart) {
      uart_io.rxd := 0.U
      when(rxBaudWrap) {
        rxState := sRxData
      }
    }
    is(sRxData) {
      uart_io.rxd := (rxfifo.io.deq.bits >> rxDataIdx)(0)
      when(rxDataWrap && rxBaudWrap) {
        rxState := sRxIdle
      }
    }
  }
  rxfifo.io.deq.ready := (rxState === sRxData) && rxDataWrap && rxBaudWrap

  val sim = Module(new SimUART(uartno))

  sim.io.clock := clock
  sim.io.reset := reset

  sim.io.serial.out.bits := txfifo.io.deq.bits
  sim.io.serial.out.valid := txfifo.io.deq.valid
  txfifo.io.deq.ready := sim.io.serial.out.ready

  rxfifo.io.enq.bits := sim.io.serial.in.bits
  rxfifo.io.enq.valid := sim.io.serial.in.valid
  sim.io.serial.in.ready := rxfifo.io.enq.ready
}

class SimUART(uartno: Int) extends BlackBox(Map("UARTNO" -> IntParam(uartno))) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val serial = Flipped(new SerialIO(DATA_WIDTH))
  })

  addResource("/testchipip/vsrc/SimUART.v")
  addResource("/testchipip/csrc/SimUART.cc")
  addResource("/testchipip/csrc/uart.cc")
  addResource("/testchipip/csrc/uart.h")
}

trait CanHavePeripheryUARTWithAdapter extends HasPeripheryUART { this: BaseSubsystem =>
}

trait CanHavePeripheryUARTWithAdapterImp extends HasPeripheryUARTModuleImp {
  implicit val p: Parameters
  val outer: CanHavePeripheryUARTWithAdapter

  def connectSimUARTs() = {
    require(p(PeripheryUARTKey).size == p(UARTAdapterKey).size)
    uart.zipWithIndex.foreach{ case (dut_io, i) =>
      val uart_sim = Module(new UARTAdapter(i)(p))
      uart_sim.io.uart.txd := dut_io.txd
      dut_io.rxd := uart_sim.io.uart.rxd
    }
  }
}
