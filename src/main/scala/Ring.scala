package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink.DevNullParams
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.{SystemBus, SystemBusParams}
import freechips.rocketchip.util.{HellaPeekingArbiter, BooleanToAugmentedBoolean}

class TLRingBundle[T <: TLChannel](
    nNodes: Int, payloadTyp: T) extends Bundle {
  val ringId = UInt(log2Ceil(nNodes).W)
  val payload = payloadTyp.cloneType
  val last = Bool()

  override def cloneType =
    new TLRingBundle(nNodes, payloadTyp).asInstanceOf[this.type]
}

class TLRingInputNode[T <: TLChannel](
    nNodes: Int, payloadTyp: T, buffer: BufferParams) extends Module {
  val bundleType = new TLRingBundle(nNodes, payloadTyp)
  val io = IO(new Bundle {
    val ext_in = Flipped(Decoupled(bundleType))
    val int_in = Flipped(Decoupled(bundleType))
    val int_out = Decoupled(bundleType)
  })

  val extArb = Module(new HellaPeekingArbiter(
    bundleType, 2, (b: TLRingBundle[T]) => b.last))
  extArb.io.in <> Seq(io.ext_in, io.int_in)
  io.int_out <> buffer(extArb.io.out)
}

class TLRingOutputNode[T <: TLChannel](
    id: Int, nNodes: Int, payloadTyp: T, buffer: BufferParams) extends Module {
  val bundleType = new TLRingBundle(nNodes, payloadTyp)
  val io = IO(new Bundle {
    val ext_out = Decoupled(bundleType)
    val int_in = Flipped(Decoupled(bundleType))
    val int_out = Decoupled(bundleType)
  })

  val intIn = buffer(io.int_in)

  val intInMatch = intIn.bits.ringId === id.U
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
  val bundleType = new TLRingBundle(nNodes, payloadTyp)

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

  lazy val module = new LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nIn = edgesIn.size
    val nOut = edgesOut.size
    val nNodes = nIn + nOut
    val idBits = log2Ceil(nNodes)

    val commonBundle = TLBundleParameters.union(
      edgesIn.map(_.bundle) ++ edgesOut.map(_.bundle))

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

    def filter[T](data: Seq[T], mask: Seq[Boolean]) =
      (data zip mask).filter(_._2).map(_._1)

    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))
    val reachabilityMatrix = edgesIn.map(
      edgeIn => edgesOut.map(
        edgeOut => edgeIn.client.clients.exists(
          client => edgeOut.manager.managers.exists(
            man => client.visibility.exists(
              caddr => man.address.exists(
                maddr => caddr.overlaps(maddr)))))))
    val canRelease = edgesIn.map(_.client.anySupportProbe)
    val canProbe = edgesOut.map(_.manager.anySupportAcquireB)

    def wrap[T <: TLChannel](
        ring: DecoupledIO[TLRingBundle[T]], tl: DecoupledIO[T],
        selects: Seq[Bool], ids: Seq[UInt], edge: TLEdge,
        sourceStart: BigInt = -1, sinkStart: BigInt = -1,
        connect: Boolean = true) {
      if (connect) {
        ring.valid := tl.valid
        ring.bits.ringId := Mux1H(selects, ids)
        ring.bits.payload := tl.bits
        ring.bits.last := edge.last(tl)

        if (sourceStart != -1 || sinkStart != -1) {
          (ring.bits.payload, tl.bits) match {
            case (ringA: TLBundleA, tlA: TLBundleA) =>
              ringA.source := tlA.source | sourceStart.U
            case (ringC: TLBundleC, tlC: TLBundleC) =>
              ringC.source := tlC.source | sourceStart.U
            case (ringD: TLBundleD, tlD: TLBundleD) =>
              ringD.sink := tlD.sink | sinkStart.U
          }
        }

        tl.ready := ring.ready
      } else {
        ring.valid := false.B
        ring.bits := DontCare
        tl.ready := false.B
      }
    }

    def trim(id: UInt, size: Int) =
      if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

    def unwrap[T <: TLChannel](
        tl: DecoupledIO[T], ring: DecoupledIO[TLRingBundle[T]],
        idSize: Int = 0,
        connect: Boolean = true) {
      if (connect) {
        tl.valid := ring.valid
        tl.bits := ring.bits.payload
        if (idSize > 0) {
          (tl.bits, ring.bits.payload) match {
            case (tlB: TLBundleB, ringB: TLBundleB) =>
              tlB.source := trim(ringB.source, idSize)
            case (tlD: TLBundleD, ringD: TLBundleD) =>
              tlD.source := trim(ringD.source, idSize)
            case (tlE: TLBundleE, ringE: TLBundleE) =>
              tlE.sink := trim(ringE.sink, idSize)
          }
        }
        ring.ready := tl.ready
      } else {
        tl.valid := false.B
        tl.bits := DontCare
        ring.ready := false.B
      }
    }

    (io_in zip edgesIn).zipWithIndex.foreach {
      case ((in, edgeIn), i) =>
        val inRange = inputIdRanges(i)
        val reachable = reachabilityMatrix(i)
        val probing = reachable.zip(canProbe).map { case (r, p) => r && p}

        val outIds = (0 until nOut).map(id => (id + nIn).U(idBits.W))
        val portAddrs = edgesOut.map(_.manager.managers.flatMap(_.address))
        val routingMask = AddressDecoder(filter(portAddrs, reachable))
        val routeAddrs = portAddrs.map(seq =>
            AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
        val routeFuncs = routeAddrs.map(seq =>
            (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

        val aMatches = filter(routeFuncs, reachable).map(
          route => route(in.a.bits.address))
        val cMatches = filter(routeFuncs, probing).map(
          route => route(in.c.bits.address))
        val eMatches = filter(outputIdRanges, probing).map(
          range => range.contains(in.e.bits.sink))

        val acquireIds = filter(outIds, reachable)
        val releaseIds = filter(outIds, probing)

        assert(!in.a.valid || PopCount(aMatches) === 1.U,
          s"TLRingNetwork: Multiple or no matching routes for A channel $i")
        assert(!in.c.valid || PopCount(cMatches) === 1.U,
          s"TLRingNetwork: Multiple or no matching routes for C channel $i")
        assert(!in.e.valid || PopCount(eMatches) === 1.U,
          s"TLRingNetwork: Multiple or no matching routes for E channel $i")

        val connectBCE = canRelease(i)

        wrap(
          ring = aRing.io.in(i),
          tl = in.a,
          selects = aMatches,
          ids = acquireIds,
          edge = edgeIn,
          sourceStart = inRange.start)

        unwrap(in.b, bRing.io.out(i), inRange.size, connectBCE)

        wrap(
          ring = cRing.io.in(i),
          tl = in.c,
          selects = cMatches,
          ids = releaseIds,
          edge = edgeIn,
          sourceStart = inRange.start,
          connect = connectBCE)

        unwrap(in.d, dRing.io.out(i), inRange.size)

        wrap(
          ring = eRing.io.in(i),
          tl = in.e,
          selects = eMatches,
          ids = releaseIds,
          edge = edgeIn,
          connect = connectBCE)
    }

    (io_out zip edgesOut).zipWithIndex.foreach {
      case ((out, edgeOut), i) =>
        val outRange = outputIdRanges(i)
        val reachable = reachabilityMatrix.map(seq => seq(i))
        val probeable = reachable.zip(canRelease).map { case (r, p) => r && p }
        val inIds = (0 until nIn).map(id => id.U(idBits.W))
        val routeFuncs = inputIdRanges.map(range =>
            (source: UInt) => range.contains(source))

        val bMatches = filter(routeFuncs, probeable).map(
          route => route(out.b.bits.source))
        val dMatches = filter(routeFuncs, reachable).map(
          route => route(out.d.bits.source))

        assert(!out.b.valid || PopCount(bMatches) === 1.U,
          s"TLRingNetwork: Multiple or no matching routes for B channel $i")
        assert(!out.d.valid || PopCount(dMatches) === 1.U,
          s"TLRingNetwork: Multiple or no matching routes for D channel $i")

        val grantIds = filter(inIds, reachable)
        val probeIds = filter(inIds, probeable)

        val connectBCE = canProbe(i)

        unwrap(out.a, aRing.io.out(i))

        wrap(
          ring = bRing.io.in(i),
          tl = out.b,
          selects = bMatches,
          ids = probeIds,
          edge = edgeOut,
          connect = connectBCE)

        unwrap(out.c, cRing.io.out(i), connect = connectBCE)

        wrap(
          ring = dRing.io.in(i),
          tl = out.d,
          selects = dMatches,
          ids = grantIds,
          edge = edgeOut,
          sinkStart = outRange.start)

        unwrap(out.e, eRing.io.out(i), outRange.size, connectBCE)
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
