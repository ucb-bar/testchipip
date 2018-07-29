package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.regmapper.{RegField, HasRegMap}
import freechips.rocketchip.subsystem.{CacheBlockBytes, BaseSubsystem}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class MemBenchParams(
  nWorkers: Int = 1,
  nXacts: Int = 8,
  qDepth: Int = 8)

case object MemBenchKey extends Field[MemBenchParams]

class MemBenchRequest extends Bundle {
  val addr     = UInt(64.W)
  val len      = UInt(32.W)
  val npasses  = UInt(16.W)
  val stride   = UInt(8.W)
  val size     = UInt(8.W)
  val inflight = UInt(8.W)
  val write    = Bool()
  val worker   = UInt(8.W)
}

trait MemBenchControllerBundle extends Bundle {
  val req = Decoupled(new MemBenchRequest)
  val resp = Flipped(Decoupled(UInt(64.W)))
}

class MemBenchIO extends MemBenchControllerBundle

trait MemBenchControllerModule extends HasRegMap {
  implicit val p: Parameters

  val io: MemBenchControllerBundle

  val addr     = Reg(UInt(64.W))
  val len      = Reg(UInt(32.W))
  val npasses  = Reg(UInt(16.W))
  val stride   = Reg(UInt(8.W))
  val size     = Reg(UInt(8.W))
  val inflight = Reg(UInt(8.W))
  val write    = Reg(Bool())
  val worker   = Wire(Decoupled(UInt(8.W)))

  io.req.valid := worker.valid
  worker.ready := io.req.ready
  io.req.bits.addr     := addr
  io.req.bits.len      := len
  io.req.bits.npasses  := npasses
  io.req.bits.stride   := stride
  io.req.bits.size     := size
  io.req.bits.inflight := inflight
  io.req.bits.write    := write
  io.req.bits.worker   := worker.bits

  val qDepth = p(MemBenchKey).qDepth
  val respQueue = Module(new Queue(UInt(64.W), qDepth))

  respQueue.io.enq <> io.resp

  regmap(
    0x00 -> Seq(RegField(64, addr)),
    0x08 -> Seq(RegField(32, len)),
    0x0C -> Seq(RegField(16, npasses)),
    0x0E -> Seq(RegField(8,  stride)),
    0x0F -> Seq(RegField(8,  size)),
    0x10 -> Seq(RegField(8,  inflight)),
    0x11 -> Seq(RegField(1,  write)),
    0x12 -> Seq(RegField.w(8, worker)),
    0x14 -> Seq(RegField.r(16, respQueue.io.count)),
    0x18 -> Seq(RegField.r(64, respQueue.io.deq)))
}

class MemBenchController(address: BigInt, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    address, "mem-bench-controller", Seq("ucbbar,mem-bench"),
    beatBytes = beatBytes)(
      new TLRegBundle((), _) with MemBenchControllerBundle)(
      new TLRegModule((), _, _) with MemBenchControllerModule)

class MemBenchWorker(implicit p: Parameters) extends LazyModule {
  val config = p(MemBenchKey)
  val node = TLHelper.makeClientNode(
    name = "mem-bench", sourceId = IdRange(0, config.nXacts))
  lazy val module = new MemBenchWorkerModule(this)
}

class MemBenchWorkerModule(outer: MemBenchWorker) extends LazyModuleImp(outer) {
  val io = IO(Flipped(new MemBenchIO))

  val (tl, edge) = outer.node.out(0)
  val sourceBits = tl.params.sourceBits
  val dataBits = tl.params.dataBits
  val beatBytes = dataBits / 8
  val blockBytes = p(CacheBlockBytes)
  val beatsPerBlock = blockBytes / beatBytes

  val req = Reg(new MemBenchRequest)
  val curaddr = Reg(UInt(64.W))
  val curlen  = Reg(UInt(32.W))

  val nXacts = outer.config.nXacts
  val xactBusy = RegInit(0.U(nXacts.W))
  val xactOnehot = PriorityEncoderOH(~xactBusy)
  val xactId = OHToUInt(xactOnehot)
  val xactIdReg = Reg(UInt(sourceBits.W))

  val aFirst = edge.first(tl.a)
  val aLast  = edge.last(tl.a)
  val dLast  = edge.last(tl.d)

  xactBusy := (xactBusy |
    Mux(tl.a.fire() && aFirst, xactOnehot, 0.U)) &
    ~Mux(tl.d.fire() && dLast, UIntToOH(tl.d.bits.source), 0.U)

  val putAcq = edge.Put(
    fromSource = Mux(aFirst, xactId, xactIdReg),
    toAddress = curaddr,
    lgSize = req.size,
    data = curaddr)._2

  val getAcq = edge.Get(
    fromSource = xactId,
    toAddress = curaddr,
    lgSize = req.size)._2

  val cycle = Reg(UInt(64.W))
  val s_idle :: s_send :: s_wait :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  io.req.ready := state === s_idle
  tl.a.valid := (state === s_send) && (PopCount(xactBusy) < req.inflight)
  tl.a.bits  := Mux(req.write, putAcq, getAcq)
  tl.d.ready := xactBusy.orR
  io.resp.valid := state === s_resp
  io.resp.bits  := cycle

  when (io.req.fire()) {
    req := io.req.bits
    curaddr := io.req.bits.addr
    curlen  := io.req.bits.len
    cycle := 0.U
    state := s_send
  }

  when (state.isOneOf(s_send, s_wait)) { cycle := cycle + 1.U }

  when (tl.a.fire()) {
    when (aFirst) {
      xactIdReg := xactId
    }
    when (aLast) {
      curaddr := curaddr + req.stride
      curlen  := curlen  - req.stride
      when (curlen === req.stride) {
        when (req.npasses === 1.U) {
          state := s_wait
        } .otherwise {
          curaddr := req.addr
          curlen  := req.len
          req.npasses := req.npasses - 1.U
        }
      }
    }
  }

  when (state === s_wait && !xactBusy.orR) { state := s_resp }
  when (io.resp.fire()) { state := s_idle }
}

class MemBenchRouter(implicit p: Parameters) extends Module {
  val nWorkers = p(MemBenchKey).nWorkers
  val io = IO(new Bundle {
    val in = Flipped(new MemBenchIO)
    val out = Vec(nWorkers, new MemBenchIO)
  })

  io.in.req.ready := false.B

  io.out.zipWithIndex.foreach { case (out, i) =>
    val me = io.in.req.bits.worker === i.U
    out.req.valid := io.in.req.valid && me
    out.req.bits  := io.in.req.bits
    when (me) { io.in.req.ready := out.req.ready }
  }

  val respArb = Module(new RRArbiter(UInt(64.W), nWorkers))
  respArb.io.in <> io.out.map(_.resp)
  io.in.resp <> respArb.io.out
}

class MemBench(address: BigInt, beatBytes: Int)(implicit p: Parameters)
    extends LazyModule {
  val nWorkers = p(MemBenchKey).nWorkers
  val controller = LazyModule(new MemBenchController(address, beatBytes))
  val workers = Seq.fill(nWorkers) { LazyModule(new MemBenchWorker) }

  val mmionode = controller.node
  val dmanode = TLIdentityNode()

  workers.foreach { worker => dmanode := worker.node }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {})

    val router = Module(new MemBenchRouter)
    router.io.in <> controller.module.io
    workers.zip(router.io.out).foreach { case (worker, out) =>
      worker.module.io <> out
    }
  }
}

trait HasPeripheryMemBench { this: BaseSubsystem =>
  private val portName = "mem-bench"
  val memBench = LazyModule(new MemBench(0x10019000, pbus.beatBytes))

  pbus.toVariableWidthSlave(Some(portName)) { memBench.mmionode }
  sbus.fromPort(Some(portName))() :=* memBench.dmanode
}
