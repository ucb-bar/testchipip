package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{SystemBus, SystemBusParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{HellaPeekingArbiter, BooleanToAugmentedBoolean}

class MeshNode[T <: Data](
    inId: Int, outId: Int,
    nInputs: Int, nOutputs: Int,
    payloadTyp: T, buffer: BufferParams) extends Module {

  // Dimensional order routing
  // Packets get passed from left to right until output column is reached
  // Then it gets send from top to bottom
  // Inputs numbered from bottom up
  // Outputs numbered from left to right
  val hasTop = inId < (nInputs - 1)
  val hasRight = outId < (nOutputs - 1)
  val bundleType = new NetworkBundle(nOutputs, payloadTyp)

  val io = IO(new Bundle {
    val top = hasTop.option(Flipped(Decoupled(bundleType)))
    val bottom = Decoupled(bundleType)
    val left = Flipped(Decoupled(bundleType))
    val right = hasRight.option(Decoupled(bundleType))
  })

  val l2b = Wire(Decoupled(bundleType))
  val routeDown = io.left.bits.netId === outId.U

  l2b.valid := io.left.valid && routeDown
  l2b.bits  := io.left.bits

  if (hasRight) {
    val l2r = Wire(Decoupled(bundleType))

    l2r.valid := io.left.valid && !routeDown
    l2r.bits  := io.left.bits
    io.left.ready := Mux(routeDown, l2b.ready, l2r.ready)

    io.right.get <> buffer(l2r)
  } else {
    io.left.ready := l2b.ready && routeDown
    assert(!io.left.valid || routeDown,
      "TileLink message reached end of mesh without finding output")
  }

  val downInputs = Seq(io.top.toSeq, Seq(l2b)).flatten
  val downArb = Module(new HellaPeekingArbiter(
    bundleType, downInputs.size, (n: NetworkBundle[T]) => n.last))
  downArb.io.in <> downInputs
  io.bottom <> buffer(downArb.io.out)

}

class Mesh[T <: Data](
    nInputs: Int, nOutputs: Int, payloadTyp: T, buffer: BufferParams)
    extends Module {
  val bundleType = new NetworkBundle(nOutputs, payloadTyp)

  val io = IO(new Bundle {
    val in = Flipped(Vec(nInputs, Decoupled(bundleType)))
    val out = Vec(nOutputs, Decoupled(bundleType))
  })

  val nodes = Seq.tabulate(nInputs) { i => Seq.tabulate(nOutputs) { o =>
    Module(new MeshNode(i, o, nInputs, nOutputs, payloadTyp, buffer))
  }}

  for (i <- 0 until (nInputs-1)) {
    val below = nodes(i)
    val above = nodes(i+1)

    below.zip(above).foreach { case (b, a) =>
      b.io.top.get <> a.io.bottom
    }
  }

  for (i <- 0 until nInputs) {
    for (o <- 0 until (nOutputs-1)) {
      val left = nodes(i)(o)
      val right = nodes(i)(o+1)

      right.io.left <> left.io.right.get
    }
    nodes(i)(0).io.left <> io.in(i)
  }

  io.out <> nodes.head.map(_.io.bottom)
}

class TLMeshNetwork(
    buffer: TLNetworkBufferParams = TLNetworkBufferParams.default)
    (implicit p: Parameters) extends LazyModule {
  val node = TLNexusNode(
    clientFn  = { seq =>
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        })
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Ring data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        })
    })

  lazy val module = new LazyModuleImp(this) with HasTLNetwork {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nInputs = edgesIn.size
    val nOutputs = edgesOut.size
    val inIdBits = if (nInputs == 1) 1 else log2Ceil(nInputs)
    val outIdBits = if (nOutputs == 1) 1 else log2Ceil(nOutputs)

    val forwardIds = (0 until nOutputs).map(_.U(outIdBits.W))
    val backwardIds = (0 until nInputs).map(_.U(inIdBits.W))
    val networkName = "TLMeshNetwork"

    if (nInputs > 1 || nOutputs > 1) {
      val aMesh = Module(new Mesh(
        nInputs, nOutputs, new TLBundleA(commonBundle), buffer.a))

      val bMesh = Module(new Mesh(
        nOutputs, nInputs, new TLBundleB(commonBundle), buffer.b))

      val cMesh = Module(new Mesh(
        nInputs, nOutputs, new TLBundleC(commonBundle), buffer.c))

      val dMesh = Module(new Mesh(
        nOutputs, nInputs, new TLBundleD(commonBundle), buffer.d))

      val eMesh = Module(new Mesh(
        nInputs, nOutputs, new TLBundleE(commonBundle), buffer.e))

      io_in.zipWithIndex.foreach { case (in, i) =>
        connectInput(i, in,
          aMesh.io.in(i),
          bMesh.io.out(i),
          cMesh.io.in(i),
          dMesh.io.out(i),
          eMesh.io.in(i))
      }

      io_out.zipWithIndex.foreach { case (out, i) =>
        connectOutput(i, out,
          aMesh.io.out(i),
          bMesh.io.in(i),
          cMesh.io.out(i),
          dMesh.io.in(i),
          eMesh.io.out(i))
      }
    } else {
      io_out.head <> io_in.head
    }
  }
}

class MeshSystemBus(params: SystemBusParams, buffer: TLNetworkBufferParams)
    (implicit p: Parameters) extends SystemBus(params) {
  private val system_bus_mesh = LazyModule(new TLMeshNetwork(buffer))

  override def inwardNode: TLInwardNode = system_bus_mesh.node
  override def outwardNode: TLOutwardNode = system_bus_mesh.node
  override def busView: TLEdge = system_bus_mesh.node.edges.in.head
}
