package testchipip.serdes

import chisel3._
import chisel3.util._
import sifive.blocks.devices.uart._

class SerialWidthAggregator(narrowW: Int, wideW: Int) extends Module {
  require(wideW > narrowW)
  require(wideW % narrowW == 0)
  val io = IO(new Bundle {
    val narrow = Flipped(Decoupled(UInt(narrowW.W)))
    val wide   = Decoupled(UInt(wideW.W))
  })

  val beats = wideW / narrowW

  val narrow_beats = RegInit(0.U(log2Ceil(beats).W))
  val narrow_last_beat = narrow_beats === (beats-1).U
  val narrow_data = Reg(Vec(beats-1, UInt(narrowW.W)))

  io.narrow.ready := Mux(narrow_last_beat, io.wide.ready, true.B)
  when (io.narrow.fire()) {
    narrow_beats := Mux(narrow_last_beat, 0.U, narrow_beats + 1.U)
    when (!narrow_last_beat) { narrow_data(narrow_beats) := io.narrow.bits }
  }
  io.wide.valid := narrow_last_beat && io.narrow.valid
  io.wide.bits := Cat(io.narrow.bits, narrow_data.asUInt)
}

class SerialWidthSlicer(narrowW: Int, wideW: Int) extends Module {
  require(wideW > narrowW)
  require(wideW % narrowW == 0)
  val io = IO(new Bundle {
    val wide   = Flipped(Decoupled(UInt(wideW.W)))
    val narrow = Decoupled(UInt(narrowW.W))
  })

  val beats = wideW / narrowW
  val wide_beats = RegInit(0.U(log2Ceil(beats).W))
  val wide_last_beat = wide_beats === (beats-1).U

  io.narrow.valid := io.wide.valid
  io.narrow.bits := io.wide.bits.asTypeOf(Vec(beats, UInt(narrowW.W)))(wide_beats)
  when (io.narrow.fire()) {
    wide_beats := Mux(wide_last_beat, 0.U, wide_beats + 1.U)
  }
  io.wide.ready := wide_last_beat && io.narrow.ready
}

class SerialWidthAdapter(narrowW: Int, wideW: Int) extends Module {
  require(wideW > narrowW)
  require(wideW % narrowW == 0)
  val io = IO(new Bundle {
    val narrow = new SerialIO(narrowW)
    val wide = new SerialIO(wideW)
  })

  val aggregator = Module(new SerialWidthAggregator(narrowW, wideW))
  aggregator.io.narrow <> io.narrow.in
  io.wide.out <> aggregator.io.wide

  val slicer = Module(new SerialWidthSlicer(narrowW, wideW))
  slicer.io.wide <> io.wide.in
  io.narrow.out <> slicer.io.narrow
}

class UARTToSerial(freqHz: BigInt, uartParams: UARTParams) extends Module {
  val io = IO(new Bundle {
    val uart = new UARTPortIO(uartParams)
    val serial = new SerialIO(8)
    val dropped = Output(Bool()) // No flow control, so dropping a beat means we're screwed
  })

  val rxm = Module(new UARTRx(uartParams))
  val rxq = Module(new Queue(UInt(uartParams.dataBits.W), uartParams.nRxEntries))
  val txm = Module(new UARTTx(uartParams))
  val txq = Module(new Queue(UInt(uartParams.dataBits.W), uartParams.nTxEntries))

  val div = (freqHz / uartParams.initBaudRate).toInt

  val dropped = RegInit(false.B)
  io.dropped := dropped
  rxm.io.en := true.B
  rxm.io.in := io.uart.rxd
  rxm.io.div := div.U
  rxq.io.enq.valid := rxm.io.out.valid
  rxq.io.enq.bits := rxm.io.out.bits
  when (rxq.io.enq.valid) { assert(rxq.io.enq.ready) }
  when (rxq.io.enq.valid && !rxq.io.enq.ready) { dropped := true.B } // no flow control
  dontTouch(rxm.io)

  txm.io.en := true.B
  txm.io.in <> txq.io.deq
  txm.io.div := div.U
  txm.io.nstop := 0.U
  io.uart.txd := txm.io.out
  dontTouch(txm.io)

  io.serial.out <> rxq.io.deq
  txq.io.enq <> io.serial.in
}
