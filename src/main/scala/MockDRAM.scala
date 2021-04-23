package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

import freechips.rocketchip.subsystem._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._


case class MockDRAMParams(
  base: BigInt,
  mock_size: BigInt,
  real_size: BigInt,
  lbp_capacity_max: Int,
  lbp_capacity_default: Int,
  lbp_latency_default: Int,
  lbp_ctrl_address: BigInt
)

case object MockDRAMKey extends Field[Option[MockDRAMParams]](None)

class WithDefaultMockDRAM extends Config((site, here, up) => {
  case MockDRAMKey => Some(MockDRAMParams(
    base = x"6000_0000",
    mock_size = 512 << 20, // 512 MB
    real_size = 4 << 10, // 4 KB
    lbp_capacity_max = 128,
    lbp_capacity_default = 64,
    lbp_latency_default = 1,
    lbp_ctrl_address = x"202_0000"
  ))
})

class DataWithCycle[T <: Data](private val t: T) extends Bundle {
  val bits = DataMirror.internal.chiselTypeClone[T](t)
  val cycle = UInt(64.W)
}

object DataWithCycle {
  def apply[T <: Data](t: T, cycle: UInt): DataWithCycle[T] = {
    val w = Wire(new DataWithCycle(t))
    w.bits := t
    w.cycle := cycle
    w
  }
}

class LBPWidget(address: BigInt, beatBytes: Int,
  max_capacity: Int, default_capacity: Int, default_latency: Int)
    (implicit p: Parameters) extends LazyModule {

  require(max_capacity >= default_capacity)

  val device = new SimpleDevice("mockdram-controller", Nil)
  val node = TLAdapterNode()
  val ctlnode = TLRegisterNode(
    address = Seq(AddressSet(address, 4096-1)),
    device = device,
    beatBytes = beatBytes)

  lazy val module = new LazyModuleImp(this) {
    val capacity = RegInit(default_capacity.U(64.W))
    val latency = RegInit(default_latency.U(16.W))
    require(node.in.size == 1)
    require(node.out.size == 1)

    val (in, edgeIn) = node.in(0)
    val (out, edgeOut) = node.out(0)

    val inflight_counter = RegInit(0.U((log2Ceil(max_capacity)+1).W))
    when (in.d.fire() && !out.d.fire()) {
      inflight_counter := inflight_counter + 1.U
    } .elsewhen (!in.d.fire() && out.d.fire()) {
      inflight_counter := inflight_counter - 1.U
    }
    val cycle = RegInit(0.U(64.W))
    cycle := cycle + 1.U

    out.a.valid := in.a.valid
    out.a.bits := in.a.bits
    in.a.ready := out.a.ready
    // Do the LBP pipe for d channel responses
    val q = Module(new Queue(new DataWithCycle(new TLBundleD(in.params)), max_capacity))
    q.io.enq.valid := out.d.valid && inflight_counter <= capacity
    q.io.enq.bits.bits := out.d.bits
    q.io.enq.bits.cycle := cycle + latency
    out.d.ready := q.io.enq.ready && inflight_counter <= capacity

    in.d.valid := q.io.deq.valid && q.io.deq.bits.cycle <= cycle
    in.d.bits := q.io.deq.bits.bits
    q.io.deq.ready := in.d.ready && q.io.deq.bits.cycle <= cycle


    // TODO we might want to support caching clients in the future
    require(!edgeOut.manager.anySupportAcquireB && !edgeOut.client.anySupportProbe)
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
    out.b.ready := true.B
    out.c.valid := false.B
    out.e.valid := false.B

    ctlnode.regmap(
      0x0 -> RegField.bytes(capacity),
      0x8 -> RegField.bytes(latency)
    )
  }
}

trait CanHaveMockDRAM { this: BaseSubsystem =>
  p(MockDRAMKey).map { params =>
    val portName = "MockDRAM"
    mbus {
      require(isPow2(params.mock_size))
      require(isPow2(params.real_size))
      val mock_aset = AddressSet(params.base, params.mock_size-1)
      val real_aset = AddressSet(params.base, params.real_size-1)
      // Currently requires cbus and mbus on same clock
      val lbp_pipe = LazyModule(new LBPWidget(
        params.lbp_ctrl_address,
        cbus.beatBytes,
        params.lbp_capacity_max,
        params.lbp_capacity_default,
        params.lbp_latency_default
      ))
      lbp_pipe.ctlnode := cbus.coupleTo("mockdram_ctrl") { TLBuffer(1) := TLFragmenter(cbus) := _ }
      val repl_params = ReplicatedRegion(real_aset, mock_aset)
      val region_replicator = LazyModule(new RegionReplicator(repl_params))
      val prefix_node = BundleBroadcast[UInt](registered = false, default = Some(() => repl_params.replicationMask.U))

      region_replicator.prefix := prefix_node
      val ram = LazyModule(new TLRAM(address=real_aset, beatBytes=mbus.beatBytes, devName=Some("mock-dram")))
        mbus.toVariableWidthSlave(Some(portName)) { ram.node := region_replicator.node := lbp_pipe.node }
    }
  }
}
