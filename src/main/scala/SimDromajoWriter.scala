package testchipip

import chisel3._
import chisel3.util._
import chisel3.core.{IntParam, StringParam}

class SimDromajoWriter extends BlackBox with HasBlackBoxResource
{
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val valid = Input(Bool())
    val addr  = Input(UInt(32.W))
    val size  = Input(UInt(3.W)) // log2 bytes size
    val data  = Input(UInt(64.W))
  })

  addResource("/testchipip/vsrc/SimDromajoWriter.v")
  addResource("/testchipip/csrc/SimDromajoWriter.cc")
}
