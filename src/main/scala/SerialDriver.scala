package testchipip

import Chisel._
import junctions._

abstract class SerialDriver(w: Int) extends Module {
  val io = new Bundle {
    val serial = new SerialIO(w).flip
    val exit = Bool(OUTPUT)
  }
}

class SimSerial(w: Int) extends BlackBox {
  val io = new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
    val serial = new SerialIO(w).flip
    val exit = Bool(OUTPUT)
  }
}

class SimSerialWrapper(w: Int) extends SerialDriver(w) {
  val bbox = Module(new SimSerial(w))
  bbox.io.clock := clock
  bbox.io.reset := reset
  bbox.io.serial <> io.serial
  io.exit := bbox.io.exit
}


