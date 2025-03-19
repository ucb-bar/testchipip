package testchipip.ctc

import chisel3._

object CTC {
  val INNER_WIDTH = 32
  val OUTER_WIDTH = 4
}

object CTCCommand {
  val read_req = 0.U
  val write_req = 1.U
  val read_ack = 2.U
  val write_ack = 3.U
}