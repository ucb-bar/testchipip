package testchipip

import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.coreplex.{HasSystemBus, HasPeripheryBus}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegisterReadIO, RegField, HasRegMap}
import freechips.rocketchip.rocket.PAddrBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{ParameterizedBundle, DecoupledHelper, UIntIsOneOf}
import scala.math.max

case class BlockDeviceConfig(nTrackers: Int = 1)

case object BlockDeviceKey extends Field[BlockDeviceConfig]

trait HasBlockDeviceParameters {
  implicit val p: Parameters
  val blockDevExternal = p(BlockDeviceKey)
  val dataBytes = 512
  val sectorBits = 32
  val nTrackers = blockDevExternal.nTrackers
  val tagBits = log2Up(nTrackers)
  val nTrackerBits = log2Up(nTrackers+1)
  val dataBitsPerBeat = 64
  val dataBeats = (dataBytes * 8) / dataBitsPerBeat
  val sectorSize = log2Ceil(sectorBits/8)
  val beatIdxBits = log2Ceil(dataBeats)
  val pAddrBits = p(PAddrBits)
}

abstract class BlockDeviceBundle(implicit val p: Parameters)
  extends ParameterizedBundle()(p) with HasBlockDeviceParameters

abstract class BlockDeviceModule(implicit val p: Parameters)
  extends Module with HasBlockDeviceParameters

class BlockDeviceRequest(implicit p: Parameters) extends BlockDeviceBundle {
  val write = Bool()
  val addr = UInt(pAddrBits.W)
  val offset = UInt(sectorBits.W)
  val len = UInt(sectorBits.W)
  val tag = UInt(tagBits.W)
}

class BlockDeviceData(implicit p: Parameters) extends BlockDeviceBundle {
  val data = UInt(dataBitsPerBeat.W)
  val tag = UInt(tagBits.W)
}

class BlockDeviceInfo(implicit p: Parameters) extends BlockDeviceBundle {
  val nsectors = UInt(sectorBits.W)
  val max_req_len = UInt(sectorBits.W)
}

class BlockDeviceIO(implicit p: Parameters) extends BlockDeviceBundle {
  val req = Decoupled(new BlockDeviceRequest)
  val data = Decoupled(new BlockDeviceData)
  val resp = Flipped(Decoupled(new BlockDeviceData))
  val info = Input(new BlockDeviceInfo)
}

class BlockDeviceArbiter(implicit p: Parameters) extends BlockDeviceModule {
  val io = IO(new Bundle {
    val in = Flipped(Vec(nTrackers, new BlockDeviceIO))
    val out = new BlockDeviceIO
  })

  val reqArb = Module(new RRArbiter(new BlockDeviceRequest, nTrackers))
  reqArb.io.in <> io.in.map(_.req)
  io.out.req <> reqArb.io.out
  io.out.req.bits.tag := reqArb.io.chosen

  val dataArb = Module(new RRArbiter(new BlockDeviceData, nTrackers))
  dataArb.io.in <> io.in.map(_.data)
  io.out.data <> dataArb.io.out
  io.out.data.bits.tag := dataArb.io.chosen

  io.out.resp.ready := false.B
  io.in.zipWithIndex.foreach { case (in, i) =>
    val me = io.out.resp.bits.tag === i.U
    in.resp.valid := me && io.out.resp.valid
    in.resp.bits := io.out.resp.bits
    when (me) { io.out.resp.ready := in.resp.ready }
  }
}

class BlockDeviceTracker(id: Int)(implicit p: Parameters)
    extends LazyModule with HasBlockDeviceParameters {

  val node = TLClientNode(TLClientParameters(
    name = s"blkdev-tracker$id", sourceId = IdRange(0, 1)))

  lazy val module = new BlockDeviceTrackerModule(this)
}

class BlockDeviceTrackerIO(implicit p: Parameters) extends BlockDeviceBundle {
  val req = Decoupled(new BlockDeviceRequest)
  val complete = Flipped(Decoupled(Bool()))
}

class BlockDeviceTrackerModule(outer: BlockDeviceTracker)
    extends LazyModuleImp(outer) with HasBlockDeviceParameters {
  val io = IO(new Bundle {
    val front = Flipped(new BlockDeviceTrackerIO)
    val mem = outer.node.bundleOut
    val bdev = new BlockDeviceIO
  })

  val tl = io.mem.head
  val req = Reg(new BlockDeviceRequest)

  require (tl.a.bits.data.getWidth == dataBitsPerBeat)

  val (s_idle :: s_bdev_req :: s_bdev_read_data ::
       s_bdev_write_data :: s_bdev_write_resp ::
       s_mem_write_resp :: s_mem_read_req ::
       s_complete :: Nil) = Enum(8)
  val state = RegInit(s_idle)

  val cacheBlockBytes = p(CacheBlockBytes)
  val blocksPerSector = dataBytes / cacheBlockBytes
  val beatsPerBlock = (cacheBlockBytes * 8) / dataBitsPerBeat

  val edge = outer.node.edgesOut(0)
  val get_acq = edge.Get(
    fromSource = 0.U,
    toAddress = req.addr,
    lgSize = log2Ceil(cacheBlockBytes).U)._2
  val put_acq = edge.Put(
    fromSource = 0.U,
    toAddress = req.addr,
    lgSize = log2Ceil(cacheBlockBytes).U,
    data = io.bdev.resp.bits.data)._2

  io.front.req.ready := state === s_idle
  io.bdev.req.valid := state === s_bdev_req
  io.bdev.req.bits := req
  io.bdev.req.bits.tag := 0.U
  io.bdev.data.valid := (state === s_bdev_write_data) && tl.d.valid
  io.bdev.data.bits.data := tl.d.bits.data
  io.bdev.data.bits.tag := 0.U
  tl.d.ready := (state === s_bdev_write_data && io.bdev.data.ready) ||
                (state === s_mem_write_resp)
  io.bdev.resp.ready := (state === s_bdev_write_resp) ||
                        (state === s_bdev_read_data && tl.a.ready)
  tl.a.valid := (state === s_mem_read_req) ||
                (state === s_bdev_read_data && io.bdev.resp.valid)
  tl.a.bits := Mux(state === s_mem_read_req, get_acq, put_acq)
  io.front.complete.valid := state === s_complete

  tl.b.ready := false.B
  tl.c.valid := false.B
  tl.e.valid := false.B

  when (io.front.req.fire()) {
    req := io.front.req.bits
    state := s_bdev_req
  }

  when (io.bdev.req.fire()) {
    when (req.write) {
      state := s_mem_read_req
    } .otherwise {
      state := s_bdev_read_data
    }
  }

  when (tl.a.ready && state === s_mem_read_req) {
    state := s_bdev_write_data
  }

  val (read_beat, read_blk_done) = Counter(io.bdev.data.fire(), beatsPerBlock)
  val (read_block, read_sector_done) = Counter(read_blk_done, blocksPerSector)

  when (read_blk_done) {
    req.addr := req.addr + cacheBlockBytes.U
    state := s_mem_read_req
  }
  when (read_sector_done) {
    req.len := req.len - 1.U
    req.offset := req.offset + 1.U
    when (req.len === 1.U) { state := s_bdev_write_resp }
  }

  when (io.bdev.resp.valid && state === s_bdev_write_resp) {
    state := s_complete
  }

  val (write_beat, write_blk_done) = Counter(
    io.bdev.resp.fire() && state === s_bdev_read_data, beatsPerBlock)
  when (write_blk_done) { state := s_mem_write_resp }

  val tl_write_d_fire = tl.d.valid && state === s_mem_write_resp
  val (write_block, write_sector_done) = Counter(tl_write_d_fire, blocksPerSector)

  when (tl_write_d_fire) {
    req.addr := req.addr + cacheBlockBytes.U
    state := s_bdev_read_data
  }

  when (write_sector_done) {
    req.len := req.len - 1.U
    req.offset := req.offset + 1.U
    when (req.len === 1.U) { state := s_complete }
  }

  when (io.front.complete.fire()) { state := s_idle }
}

class BlockDeviceBackendIO(implicit p: Parameters) extends BlockDeviceBundle {
  val req = Decoupled(new BlockDeviceRequest)
  val allocate = Flipped(Decoupled(UInt(tagBits.W)))
  val nallocate = Input(UInt(log2Ceil(nTrackers+1).W))
  val complete = Flipped(Decoupled(UInt(tagBits.W)))
  val ncomplete = Input(UInt(log2Ceil(nTrackers+1).W))
}

class BlockDeviceRouter(implicit p: Parameters) extends BlockDeviceModule {
  val io = IO(new Bundle {
    val in = Flipped(new BlockDeviceBackendIO)
    val out = Vec(nTrackers, new BlockDeviceTrackerIO)
  })

  val outReadyAll = io.out.map(_.req.ready)
  val outReadyOH = PriorityEncoderOH(outReadyAll)
  val outReady = outReadyAll.reduce(_ || _)

  val qDepth = max(2, nTrackers)
  val allocQueue = Module(new Queue(UInt(tagBits.W), qDepth))
  io.in.allocate <> allocQueue.io.deq
  io.in.nallocate := PopCount(outReadyAll)

  val helper = DecoupledHelper(
    outReady,
    io.in.req.valid,
    allocQueue.io.enq.ready)

  io.in.req.ready := helper.fire(io.in.req.valid)
  allocQueue.io.enq.valid := helper.fire(allocQueue.io.enq.ready)
  allocQueue.io.enq.bits := OHToUInt(outReadyOH)

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.req.valid := helper.fire(outReady, outReadyOH(i))
    out.req.bits := io.in.req.bits
  }

  val completeQueue = Module(new Queue(UInt(tagBits.W), qDepth))
  val completeArb = Module(new RRArbiter(Bool(), nTrackers))
  completeArb.io.in <> io.out.map(_.complete)
  completeQueue.io.enq.valid := completeArb.io.out.valid
  completeQueue.io.enq.bits := completeArb.io.chosen
  completeArb.io.out.ready := completeQueue.io.enq.ready
  io.in.complete <> completeQueue.io.deq
  io.in.ncomplete := completeQueue.io.count
}

case class BlockDeviceFrontendParams(
  address: BigInt, beatBytes: Int)

trait BlockDeviceFrontendBundle extends Bundle with HasBlockDeviceParameters {
  implicit val p: Parameters

  val back = new BlockDeviceBackendIO
  val info = Input(new BlockDeviceInfo)
}

trait BlockDeviceFrontendModule extends Module
    with HasRegMap with HasBlockDeviceParameters {

  implicit val p: Parameters
  val io: BlockDeviceFrontendBundle
  def params: BlockDeviceFrontendParams
  val dataBits = params.beatBytes * 8

  require (dataBits >= sectorBits)
  require (dataBits >= pAddrBits)

  val addr = Reg(UInt(pAddrBits.W))
  val offset = Reg(UInt(sectorBits.W))
  val len = Reg(UInt(sectorBits.W))
  val write = Reg(Bool())

  val allocRead = Wire(new RegisterReadIO(UInt(tagBits.W)))
  io.back.req.valid := allocRead.request.valid
  io.back.req.bits.addr := addr
  io.back.req.bits.offset := offset
  io.back.req.bits.len := len
  io.back.req.bits.write := write
  allocRead.request.ready := io.back.req.ready
  allocRead.response <> io.back.allocate

  interrupts(0) := io.back.complete.valid

  regmap(
    0x00 -> Seq(RegField(pAddrBits, addr)),
    0x04 -> Seq(RegField(sectorBits, offset)),
    0x08 -> Seq(RegField(sectorBits, len)),
    0x0C -> Seq(RegField(1, write)),
    0x10 -> Seq(RegField.r(tagBits, allocRead)),
    0x14 -> Seq(RegField.r(nTrackerBits, io.back.nallocate)),
    0x18 -> Seq(RegField.r(tagBits, io.back.complete)),
    0x1C -> Seq(RegField.r(nTrackerBits, io.back.ncomplete)),
    0x20 -> Seq(RegField.r(sectorBits, io.info.nsectors)),
    0x24 -> Seq(RegField.r(sectorBits, io.info.max_req_len)))
}

class BlockDeviceFrontend(c: BlockDeviceFrontendParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "blkdev-controller", Seq("ucbbar,blkdev"),
    interrupts = 1, beatBytes = c.beatBytes, concurrency = 1)(
      new TLRegBundle(c, _)    with BlockDeviceFrontendBundle)(
      new TLRegModule(c, _, _) with BlockDeviceFrontendModule)

class BlockDeviceController(address: BigInt, beatBytes: Int)(implicit p: Parameters)
    extends LazyModule with HasBlockDeviceParameters {

  val mmio = TLInputNode()
  val mem = TLOutputNode()
  val trackers = Seq.tabulate(nTrackers)(
    id => LazyModule(new BlockDeviceTracker(id)))
  val frontend = LazyModule(new BlockDeviceFrontend(
    BlockDeviceFrontendParams(address, beatBytes)))
  val intnode = IntOutputNode()

  frontend.node := mmio
  intnode := frontend.intnode
  trackers.foreach { tr => mem := tr.node }

  lazy val module = new BlockDeviceControllerModule(this)
}

class BlockDeviceControllerModule(outer: BlockDeviceController)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val mmio = outer.mmio.bundleIn
    val mem = outer.mem.bundleOut
    val bdev = new BlockDeviceIO
    val interrupt = outer.intnode.bundleOut
  })

  val frontend = outer.frontend.module
  val router = Module(new BlockDeviceRouter)
  val trackers = outer.trackers.map(_.module)
  val arbiter = Module(new BlockDeviceArbiter)

  frontend.io.info := io.bdev.info
  router.io.in <> frontend.io.back
  trackers.zip(router.io.out).foreach {
    case (tracker, out) => tracker.io.front <> out
  }
  arbiter.io.in <> trackers.map(_.io.bdev)
  io.bdev <> arbiter.io.out
}

class BlockDeviceModel(nSectors: Int)(implicit p: Parameters) extends BlockDeviceModule {
  val io = IO(Flipped(new BlockDeviceIO))

  val blocks = Mem(nSectors, Vec(dataBeats, UInt(dataBitsPerBeat.W)))
  val requests = Reg(Vec(nTrackers, new BlockDeviceRequest))
  val beatCounts = Reg(Vec(nTrackers, UInt(beatIdxBits.W)))
  val reqValid = RegInit(0.U(nTrackers.W))

  when (io.req.fire()) {
    requests(io.req.bits.tag) := io.req.bits
    beatCounts(io.req.bits.tag) := 0.U
  }

  val dataReq = requests(io.data.bits.tag)
  val dataBeat = beatCounts(io.data.bits.tag)
  when (io.data.fire()) {
    blocks(dataReq.offset)(dataBeat) := io.data.bits.data
    when (dataBeat === (dataBeats-1).U) {
      requests(io.data.bits.tag).offset := dataReq.offset + 1.U
      requests(io.data.bits.tag).len := dataReq.len - 1.U
      beatCounts(io.data.bits.tag) := 0.U
    } .otherwise {
      beatCounts(io.data.bits.tag) := dataBeat + 1.U
    }
  }

  val respReq = requests(io.resp.bits.tag)
  val respBeat = beatCounts(io.resp.bits.tag)
  when (io.resp.fire() && !respReq.write) {
    when (respBeat === (dataBeats-1).U) {
      requests(io.resp.bits.tag).offset := respReq.offset + 1.U
      requests(io.resp.bits.tag).len := respReq.len - 1.U
      beatCounts(io.resp.bits.tag) := 0.U
    } .otherwise {
      beatCounts(io.resp.bits.tag) := respBeat + 1.U
    }
  }

  val respValid = reqValid & Cat(
    requests.reverse.map(req => !req.write || (req.len === 0.U)))
  val respValidOH = PriorityEncoderOH(respValid)
  val respFinished = io.resp.fire() && (respReq.write ||
    (respBeat === (dataBeats-1).U && respReq.len === 1.U))

  reqValid := (reqValid |
    Mux(io.req.fire(), UIntToOH(io.req.bits.tag), 0.U)) &
    ~Mux(respFinished, respValidOH, 0.U)

  io.req.ready := !reqValid.andR
  io.data.ready := (reqValid >> io.data.bits.tag)(0) && dataReq.write
  io.resp.valid := respValid.orR
  io.resp.bits.tag := OHToUInt(respValidOH)
  io.resp.bits.data := blocks(respReq.offset)(respBeat)
  io.info.nsectors := nSectors.U
  io.info.max_req_len := ~0.U(sectorBits.W)
}

object SimBlockDeviceParamMap {
  def apply(p: Parameters) = {
    val config = p(BlockDeviceKey)
    Map(
      "ADDR_BITS" -> IntParam(p(PAddrBits)),
      "TAG_BITS" -> IntParam(log2Up(config.nTrackers)))
  }
}

class SimBlockDevice(implicit p: Parameters)
    extends BlackBox(SimBlockDeviceParamMap(p)) {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val bdev = Flipped(new BlockDeviceIO)
  })
}

trait HasPeripheryBlockDevice extends HasPeripheryBus with HasSystemBus {
  implicit val p: Parameters

  val controller = LazyModule(new BlockDeviceController(
    0x10015000, pbus.beatBytes))

  controller.mmio := pbus.toVariableWidthSlaves
  sbus.fromSyncPorts() :=* controller.mem
  ibus.fromSync := controller.intnode
}

trait HasPeripheryBlockDeviceModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryBlockDevice
  val clock: Clock
  val reset: Bool

  val bdev = IO(new BlockDeviceIO)
  bdev <> outer.controller.module.io.bdev

  def connectSimBlockDevice() {
    val sim = Module(new SimBlockDevice)
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.bdev <> bdev
  }

  def connectBlockDeviceModel() {
    val model = Module(new BlockDeviceModel(16))
    model.io <> bdev
  }
}
