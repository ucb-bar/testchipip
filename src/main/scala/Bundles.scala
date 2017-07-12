package testchipip

import chisel3._
import chisel3.util._

class SerialIO(w: Int) extends Bundle {
  val in = Flipped(Decoupled(UInt(w.W)))
  val out = Decoupled(UInt(w.W))

  override def cloneType = new SerialIO(w).asInstanceOf[this.type]
}

class StreamChannel(w: Int) extends Bundle {
  val data = UInt(w.W)
  val last = Bool()

  override def cloneType = new StreamChannel(w).asInstanceOf[this.type]
}

class StreamIO(w: Int) extends Bundle {
  val in = Flipped(Decoupled(new StreamChannel(w)))
  val out = Decoupled(new StreamChannel(w))

  override def cloneType = new StreamIO(w).asInstanceOf[this.type]
}
