package testchipip

import chisel3._
import chisel3.core.IntParam
import freechips.rocketchip.amba.axi4.{AXI4BundleParameters, AXI4Bundle}

class SimDRAM(memSize: BigInt, lineSize: Int,
              params: AXI4BundleParameters) extends BlackBox(Map(
    "MEM_SIZE" -> IntParam(memSize),
    "LINE_SIZE" -> IntParam(lineSize),
    "ADDR_BITS" -> IntParam(params.addrBits),
    "DATA_BITS" -> IntParam(params.dataBits),
    "ID_BITS" -> IntParam(params.idBits))) {

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val axi = Flipped(new AXI4Bundle(params))
  })
}
