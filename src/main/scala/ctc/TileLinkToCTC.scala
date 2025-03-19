package testchipip.ctc

import chisel3._
import chisel3.util._
import testchipip.serdes._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}

// a tl-manager (slave) device
// from inner: receives read and write requests in TL
// to outer: sends read and write requests in CTC
// from outer: receives read and write responses in CTC
// to inner: sends read and write responses in TL
class TileLinkToCTC(sinkIds: Int = 1, beatBytes: Int = 8, baseAddr: BigInt = 0, size: BigInt = 1024)
                  (implicit p: Parameters) extends LazyModule {
  val addrSet = AddressSet(baseAddr, size - 1)
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v2(
                  address = Seq(addrSet),
                  )),
    beatBytes = beatBytes)))

  lazy val module = new TileLinkToCTCModule(this)
}

class TileLinkToCTCModule(outer: TileLinkToCTC) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val flit = new DecoupledFlitIO(CTC.INNER_WIDTH)
  })

  val (mem, edge) = outer.node.in(0)
}