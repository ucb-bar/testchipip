package testchipip

import Chisel._
import uncore.tilelink._
import config.Parameters
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait HasSCRParameters {
  val scrDataBits = 64
}

class SCRFile(
    controlNames: Seq[String], statusNames: Seq[String],
    controlInits: Seq[UInt], c: Clock = null, r: Bool = null)
    (implicit p: Parameters)
    extends Module(Option(c), Option(r)) with HasSCRParameters {

  val nControl = controlNames.size
  val nStatus = statusNames.size

  val io = new Bundle {
    val tl = (new ClientUncachedTileLinkIO).flip
    val control = Vec(nControl, UInt(OUTPUT, width = scrDataBits))
    val status  = Vec(nStatus,  UInt(INPUT, width = scrDataBits))
  }

  val controlMapping = controlNames.zipWithIndex.toMap
  val statusMapping = statusNames.zipWithIndex.toMap

  def control(name: String) = io.control(controlMapping(name))
  def status(name: String) = io.status(statusMapping(name))

  require(controlInits.size == nControl)
  require(io.tl.tlDataBits == scrDataBits)

  val ctrl_reg = controlInits.map((u: UInt) => Reg(UInt(width = scrDataBits), init = u))
  val all_reg = Vec(ctrl_reg ++ io.status)

  val acq = Queue(io.tl.acquire)
  val addr = Cat(acq.bits.addr_block, acq.bits.addr_beat)
  val index = addr(log2Up(nControl+nStatus), 0)
  val wen = acq.valid && acq.bits.hasData()
  val wdata = acq.bits.data

  for (i <- 0 until nControl)
    when (wen && index === UInt(i)) { ctrl_reg(i) := wdata }

  acq.ready := io.tl.grant.ready
  io.tl.grant.valid := acq.valid
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = acq.bits.addr_beat,
    data = all_reg(index))

  io.control := ctrl_reg
}

class SCRBuilder(val devName: String) extends HasSCRParameters {
  val controlNames = new ListBuffer[String]
  val statusNames  = new ListBuffer[String]
  val controlInits = new ListBuffer[UInt]

  def addControl(name: String, init: UInt = null) {
    if(init != null) require(init.isLit)
    controlNames += name
    controlInits += init
  }

  def addStatus(name: String) {
    statusNames += name
  }

  def generate(start: BigInt, c: Clock = null, r: Bool = null)(implicit p: Parameters): SCRFile = {
    SCRHeaderOutput.add(this.makeHeader(start))
    SCRAddressMap.add(devName, this.makeHashMap(start))
    Module(new SCRFile(controlNames.toSeq, statusNames.toSeq, controlInits.toSeq, c, r))
  }

  def makeHeader(start: BigInt): String = {
    val sb = new StringBuilder
    val statusOff = controlNames.size

    for ((name, i) <- controlNames.zipWithIndex)
      sb.append(s"#define ${devName.toUpperCase}_${name.toUpperCase} ${start + (i * scrDataBits/8)}\n")

    for ((name, i) <- statusNames.zipWithIndex)
      sb.append(s"#define ${devName.toUpperCase}_${name.toUpperCase} ${start + (i * scrDataBits/8) + (statusOff * scrDataBits/8)}\n")

    sb.toString
  }

  def makeHashMap(start: BigInt): HashMap[String,BigInt] = {
    val map = new HashMap[String,BigInt]
    val statusOff = controlNames.size*(scrDataBits/8)

    for ((name, i) <- controlNames.zipWithIndex)
      map.put(name, start + i*(scrDataBits/8))

    for ((name, i) <- statusNames.zipWithIndex)
      map.put(name, start + statusOff + i*(scrDataBits/8))

    map
  }
}

object SCRAddressMap {
  val contents = new HashMap[String,HashMap[String,BigInt]]

  def add(s: String, m: HashMap[String,BigInt]) {
    contents.put(s, m)
  }

  def apply(s: String) = contents.get(s)
}

object SCRHeaderOutput {
  val contents = new ListBuffer[String]

  def add(s: String) {
    if (!SCRHeaderOutput.contents.contains(s)) SCRHeaderOutput.contents += s
  }
}
