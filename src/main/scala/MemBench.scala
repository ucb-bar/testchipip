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
  val addr   = UInt(64.W)
  val len    = UInt(32.W)
  val write  = Bool()
  val worker = UInt(8.W)
}

trait MemBenchControllerBundle extends Bundle {
  val req = Decoupled(new MemBenchRequest)
  val total = Flipped(Decoupled(UInt(64.W)))
  val cumulative = Flipped(Decoupled(UInt(64.W)))
}

class MemBenchIO extends MemBenchControllerBundle

trait MemBenchControllerModule extends HasRegMap {
  implicit val p: Parameters

  val io: MemBenchControllerBundle

  val addr   = Reg(UInt(64.W))
  val len    = Reg(UInt(32.W))
  val write  = Reg(Bool())
  val worker = Wire(Decoupled(UInt(8.W)))

  io.req.valid := worker.valid
  worker.ready := io.req.ready
  io.req.bits.addr   := addr
  io.req.bits.len    := len
  io.req.bits.write  := write
  io.req.bits.worker := worker.bits

  val qDepth = p(MemBenchKey).qDepth
  val totalQueue = Module(new Queue(UInt(64.W), qDepth))
  val cumulativeQueue = Module(new Queue(UInt(64.W), qDepth))

  totalQueue.io.enq <> io.total
  cumulativeQueue.io.enq <> io.cumulative

  regmap(
    0x00 -> Seq(RegField(64, addr)),
    0x08 -> Seq(RegField(32, len)),
    0x0C -> Seq(RegField(1, write)),
    0x0D -> Seq(RegField.w(8, worker)),
    0x0E -> Seq(RegField.r(16, totalQueue.io.count)),
    0x10 -> Seq(RegField.r(64, totalQueue.io.deq)),
    0x18 -> Seq(RegField.r(64, cumulativeQueue.io.deq)))
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

  val nXacts = outer.config.nXacts
  val xactBusy = RegInit(0.U(nXacts.W))
  val xactOnehot = PriorityEncoderOH(~xactBusy)
  val xactId = OHToUInt(xactOnehot)
  val xactIdReg = Reg(UInt(sourceBits.W))
  val xactStart = Reg(Vec(nXacts, UInt(64.W)))

  val aFirst = edge.first(tl.a)
  val aLast  = edge.last(tl.a)
  val dLast  = edge.last(tl.d)

  xactBusy := (xactBusy |
    Mux(tl.a.fire() && aFirst, xactOnehot, 0.U)) &
    ~Mux(tl.d.fire() && dLast, UIntToOH(tl.d.bits.source), 0.U)

  val putAcq = edge.Put(
    fromSource = Mux(aFirst, xactId, xactIdReg),
    toAddress = req.addr,
    lgSize = log2Ceil(blockBytes).U,
    data = req.addr)._2
    
  val getAcq = edge.Get(
    fromSource = xactId,
    toAddress = req.addr,
    lgSize = log2Ceil(blockBytes).U)._2

  val cycle = Reg(UInt(64.W))
  val cumulative = Reg(UInt(64.W))
  val s_idle :: s_send :: s_wait :: s_total :: s_cumulative :: Nil = Enum(5)
  val state = RegInit(s_idle)

  io.req.ready := state === s_idle
  tl.a.valid := (state === s_send) && !xactBusy.andR
  tl.a.bits  := Mux(req.write, putAcq, getAcq)
  tl.d.ready := xactBusy.orR
  io.total.valid := state === s_total
  io.total.bits  := cycle
  io.cumulative.valid := state === s_cumulative
  io.cumulative.bits  := cumulative

  when (io.req.fire()) {
    req := io.req.bits
    cycle := 0.U
    cumulative := 0.U
    state := s_send
  }

  when (state.isOneOf(s_send, s_wait)) { cycle := cycle + 1.U }

  when (tl.a.fire()) {
    when (aFirst) {
      xactIdReg := xactId
      xactStart(xactId) := cycle
    }
    when (aLast) {
      req.addr := req.addr + blockBytes.U
      req.len  := req.len  - blockBytes.U
      when (req.len === blockBytes.U) { state := s_wait }
    }
  }

  when (tl.d.fire() && dLast) {
    cumulative := cumulative + (cycle - xactStart(tl.d.bits.source))
  }

  when (state === s_wait && !xactBusy.orR) { state := s_total }
  when (io.total.fire()) { state := s_cumulative }
  when (io.cumulative.fire()) { state := s_idle }
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

  val totalArb = Module(new RRArbiter(UInt(64.W), nWorkers))
  totalArb.io.in <> io.out.map(_.total)
  io.in.total <> totalArb.io.out

  val cumulativeArb = Module(new RRArbiter(UInt(64.W), nWorkers))
  cumulativeArb.io.in <> io.out.map(_.cumulative)
  io.in.cumulative <> cumulativeArb.io.out
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
