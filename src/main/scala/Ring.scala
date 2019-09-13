package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{SystemBus, SystemBusParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{HellaPeekingArbiter, BooleanToAugmentedBoolean}

class TLRingInputNode[T <: TLChannel](
    nNodes: Int, payloadTyp: T, buffer: BufferParams) extends Module {
  val bundleType = new TLNetworkBundle(nNodes, payloadTyp)
  val io = IO(new Bundle {
    val ext_in = Flipped(Decoupled(bundleType))
    val int_in = Flipped(Decoupled(bundleType))
    val int_out = Decoupled(bundleType)
  })

  val extArb = Module(new HellaPeekingArbiter(
    bundleType, 2, (b: TLNetworkBundle[T]) => b.last))
  extArb.io.in <> Seq(io.ext_in, io.int_in)
  io.int_out <> buffer(extArb.io.out)
}

class TLRingOutputNode[T <: TLChannel](
    id: Int, nNodes: Int, payloadTyp: T, buffer: BufferParams) extends Module {
  val bundleType = new TLNetworkBundle(nNodes, payloadTyp)
  val io = IO(new Bundle {
    val ext_out = Decoupled(bundleType)
    val int_in = Flipped(Decoupled(bundleType))
    val int_out = Decoupled(bundleType)
  })

  val intIn = buffer(io.int_in)

  val intInMatch = intIn.bits.netId === id.U
  io.ext_out.valid := intIn.valid && intInMatch
  io.ext_out.bits := intIn.bits
  io.int_out.valid := intIn.valid && !intInMatch
  io.int_out.bits := intIn.bits
  intIn.ready := Mux(intInMatch, io.ext_out.ready, io.int_out.ready)
}

class TLRing[T <: TLChannel](
    nIn: Int, nOut: Int, payloadTyp: T,
    buffer: BufferParams, inputFirst: Boolean) extends Module {
  val nNodes = nIn + nOut
  val bundleType = new TLNetworkBundle(nNodes, payloadTyp)

  val io = IO(new Bundle {
    val in = Flipped(Vec(nIn, Decoupled(bundleType)))
    val out = Vec(nOut, Decoupled(bundleType))
  })

  val inNodes = Seq.fill(nIn) {
    Module(new TLRingInputNode(nNodes, payloadTyp, buffer))
  }

  val outIdStart = if (inputFirst) nIn else 0
  val outNodes = Seq.tabulate(nOut) { i =>
    Module(new TLRingOutputNode(i + outIdStart, nNodes, payloadTyp, buffer))
  }

  inNodes.init.zip(inNodes.tail).foreach { case (left, right) =>
    right.io.int_in <> left.io.int_out
  }

  outNodes.init.zip(outNodes.tail).foreach { case (left, right) =>
    right.io.int_in <> left.io.int_out
  }

  inNodes.head.io.int_in <> outNodes.last.io.int_out
  outNodes.head.io.int_in <> inNodes.last.io.int_out

  io.out <> outNodes.map(_.io.ext_out)
  inNodes.zip(io.in).foreach { case (node, in) => node.io.ext_in <> in }
}

class TLRingNetwork(buffer: BufferParams = BufferParams.default)
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
    val nIn = edgesIn.size
    val nOut = edgesOut.size
    val nNodes = nIn + nOut
    val idBits = log2Ceil(nNodes)

    val forwardIds = (0 until nOut).map(id => (id + nIn).U(idBits.W))
    val backwardIds = (0 until nIn).map(_.U(idBits.W))
    val networkName = "TLRingNetwork"

    val aRing = Module(new TLRing(
      nIn, nOut, new TLBundleA(commonBundle),
      buffer, true))

    val bRing = Module(new TLRing(
      nOut, nIn, new TLBundleB(commonBundle),
      buffer, false))

    val cRing = Module(new TLRing(
      nIn, nOut, new TLBundleC(commonBundle),
      buffer, true))

    val dRing = Module(new TLRing(
      nOut, nIn, new TLBundleD(commonBundle),
      buffer, false))

    val eRing = Module(new TLRing(
      nIn, nOut, new TLBundleE(commonBundle),
      buffer, true))

    io_in.zipWithIndex.foreach { case (in, i) =>
      connectInput(i, in,
        aRing.io.in(i),
        bRing.io.out(i),
        cRing.io.in(i),
        dRing.io.out(i),
        eRing.io.in(i))
    }

    io_out.zipWithIndex.foreach { case (out, i) =>
      connectOutput(i, out,
        aRing.io.out(i),
        bRing.io.in(i),
        cRing.io.out(i),
        dRing.io.in(i),
        eRing.io.out(i))
    }
  }
}

class RingSystemBus(params: SystemBusParams, buffer: BufferParams)
    (implicit p: Parameters) extends SystemBus(params) {
  private val system_bus_ring = LazyModule(new TLRingNetwork(buffer))

  override def inwardNode: TLInwardNode = system_bus_ring.node
  override def outwardNode: TLOutwardNode = system_bus_ring.node
  override def busView: TLEdge = system_bus_ring.node.edges.in.head
}
