package testchipip

import chisel3._
import junctions._

abstract class SerialDriver(w: Int) extends Module {
  val io = IO(new Bundle {
    val serial = Flipped(new SerialIO(w))
    val exit = Output(Bool())
  })
}

class SimSerial(w: Int) extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val serial = Flipped(new SerialIO(w))
    val exit = Output(Bool())
  })
}

class SimSerialWrapper(w: Int) extends SerialDriver(w) {
  val bbox = Module(new SimSerial(w))
  bbox.io.clock := clock
  bbox.io.reset := reset
  bbox.io.serial <> io.serial
  io.exit := bbox.io.exit
}


