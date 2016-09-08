package testchipip

import Chisel._
import uncore.tilelink._
import cde.Parameters
import scala.collection.mutable.HashMap

trait HasSCRParameters {
  val scrDataBits = 64
}

class SCRFile(nControl: Int, nStatus: Int, ctrlInit: Seq[UInt])(implicit p: Parameters)
    extends Module with HasSCRParameters {

  val io = new Bundle {
    val tl = (new ClientUncachedTileLinkIO).flip
    val control = Vec(nControl, UInt(OUTPUT, width = scrDataBits))
    val status = Vec(nStatus, UInt(INPUT, width = scrDataBits))
  }

  assert(io.tl.tlDataBits == scrDataBits,
    s"SCRFile TileLink port must have ${scrDataBits} data bits")

  assert(ctrlInit.size == 0 || ctrlInit.size == nControl)

  val ctrl_reg = if (ctrlInit.size == 0)
    Reg(Vec(nControl, UInt(width = scrDataBits)))
  else Reg(init = Vec(ctrlInit))

  val all_reg = Vec(ctrl_reg ++ io.status)

  val acq = Queue(io.tl.acquire)
  val addr = Cat(acq.bits.addr_block, acq.bits.addr_beat)
  val wen = acq.valid && acq.bits.hasData()
  val wdata = acq.bits.data

  when (wen) { ctrl_reg(addr) := wdata }

  acq.ready := io.tl.grant.ready
  io.tl.grant.valid := acq.valid
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = acq.bits.addr_beat,
    data = all_reg(addr))

  io.control := ctrl_reg
}

class SCRBuilder extends HasSCRParameters {
  val controlMap = new HashMap[String, UInt]
  val statusMap  = new HashMap[String, UInt]
  val initMap    = new HashMap[String, UInt]

  def status(name: String): UInt = {
    val wire = statusMap.getOrElse(name, Wire(UInt(width = scrDataBits)))
    statusMap(name) = wire
    wire
  }

  def control(name: String, init: UInt): UInt = {
    val wire = controlMap.getOrElse(name, Wire(UInt(width = scrDataBits)))
    controlMap(name) = wire
    initMap(name) = init
    wire
  }

  def control(name: String): UInt = control(name, UInt(0, scrDataBits))

  def generate(implicit p: Parameters): ClientUncachedTileLinkIO = {
    val scrfile = Module(new SCRFile(controlMap.size, statusMap.size, initMap.values.toSeq))
    for ((ctrl, i) <- controlMap.values.zipWithIndex) {
      ctrl := scrfile.io.control(i)
    }
    for ((stat, i) <- statusMap.values.zipWithIndex) {
      scrfile.io.status(i) := stat
    }
    scrfile.io.tl
  }

  def generate(tl: ClientUncachedTileLinkIO)(implicit p: Parameters) {
    generate <> tl
  }
}
