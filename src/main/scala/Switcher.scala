package testchipip

import Chisel._
import uncore.tilelink._
import cde.{Parameters, Field}

trait SwitchesTileLinkChannels {
  def connectWhen[T <: Data](sel: Bool, out: DecoupledIO[T], in: DecoupledIO[T]) {
    when (sel) {
      out.valid := in.valid
      in.ready := out.ready
      out.bits := in.bits
    }
  }

  def connectWhen(sel: Bool, out: ClientUncachedTileLinkIO, in: ClientUncachedTileLinkIO) {
    connectWhen(sel, out.acquire, in.acquire)
    connectWhen(sel, in.grant, out.grant)
  }

  def connectWhen(sel: Bool, out: ClientTileLinkIO, in: ClientTileLinkIO) {
    connectWhen(sel, out.acquire, in.acquire)
    connectWhen(sel, out.release, in.release)
    connectWhen(sel, out.finish, in.finish)
    connectWhen(sel, in.probe, out.probe)
    connectWhen(sel, in.grant, out.grant)
  }

  def disconnectIn[T <: Data](in: DecoupledIO[T]) {
    in.ready := Bool(false)
  }

  def disconnectOut[T <: Data](out: DecoupledIO[T]) {
    out.valid := Bool(false)
    out.bits := out.bits.fromBits(UInt(0))
  }

  def disconnectIn(in: ClientTileLinkIO) {
    disconnectIn(in.acquire)
    disconnectIn(in.release)
    disconnectIn(in.finish)
    disconnectOut(in.probe)
    disconnectOut(in.grant)
  }

  def disconnectOut(out: ClientTileLinkIO) {
    disconnectOut(out.acquire)
    disconnectOut(out.release)
    disconnectOut(out.finish)
    disconnectIn(out.probe)
    disconnectIn(out.grant)
  }

  def disconnectIn(in: ClientUncachedTileLinkIO) {
    disconnectIn(in.acquire)
    disconnectOut(in.grant)
  }

  def disconnectOut(out: ClientUncachedTileLinkIO) {
    disconnectOut(out.acquire)
    disconnectIn(out.grant)
  }

  val nInputChannels: Int
  val nOutputChannels: Int
  val allowedRoutesOpt: Option[Seq[Seq[Int]]]
  val allowedRoutes = allowedRoutesOpt.getOrElse(
    Seq.fill(nOutputChannels)(0 until nInputChannels))

  // Some sanity checking on allowedRoutes
  require(allowedRoutes.size == nOutputChannels,
    "Not enough outputs in allowedRoutes configuration")
  for (chan <- allowedRoutes.flatten) {
    require(chan < nInputChannels,
      s"Input channel $chan in allowedRoutes is larger than highest possible input channel")
  }
}

/** Route the input interfaces to the output interfaces
 *  based on the settings in select.
 *  Each element in select controls the routing of an input channel
 *  If io.select(X) is set to Y, then input X is routed to output Y.
 *
 *  The allowedRoutes parameter determines the set of input channels that
 *  can be routed to an output. Each element represents an output channel
 *  and is a sequence of ints representing the input channels.
 *  So if allowedRoutes(X) == (A, B), then only inputs A and B can be routed
 *  to output X. The default is that all inputs can be routed to all outputs. */
class ClientTileLinkIOSwitcher(
    val nInputChannels: Int, val nOutputChannels: Int,
    val allowedRoutesOpt: Option[Seq[Seq[Int]]] = None)
    (implicit p: Parameters) extends TLModule()(p)
    with SwitchesTileLinkChannels {
  val io = new Bundle {
    val select = Vec(nInputChannels, UInt(log2Up(nOutputChannels))).asInput
    val in = Vec(nInputChannels, new ClientTileLinkIO).flip
    val out = Vec(nOutputChannels, new ClientTileLinkIO)
  }

  def connectWhen(sels: Seq[Bool], outs: Seq[ClientTileLinkIO], ins: Seq[ClientTileLinkIO]) {
    for ((sel, out, in) <- (sels, outs, ins).zipped) {
      connectWhen(sel, out, in)
    }
  }

  def disconnectIn(ins: Seq[ClientTileLinkIO]) {
    for (in <- ins) { disconnectIn(in) }
  }

  def disconnectOut(outs: Seq[ClientTileLinkIO]) {
    for (out <- outs) { disconnectOut(out) }
  }

  disconnectIn(io.in)

  for ((out, i) <- io.out.zipWithIndex) {
    val selects = allowedRoutes(i).map(j => io.select(j) === UInt(i))
    val inputs = allowedRoutes(i).map(io.in(_))
    val arb = Module(new ClientTileLinkIOArbiter(inputs.size))
    disconnectOut(arb.io.in)
    connectWhen(selects, arb.io.in, inputs)
    out <> arb.io.out
  }
}

class ClientUncachedTileLinkIOSwitcher(
    val nInputChannels: Int, val nOutputChannels: Int,
    val allowedRoutesOpt: Option[Seq[Seq[Int]]] = None)
    (implicit p: Parameters) extends TLModule()(p)
    with SwitchesTileLinkChannels {

  val io = new Bundle {
    val select = Vec(nInputChannels, UInt(log2Up(nOutputChannels))).asInput
    val in = Vec(nInputChannels, new ClientUncachedTileLinkIO).flip
    val out = Vec(nOutputChannels, new ClientUncachedTileLinkIO)
  }

  def connectWhen(sels: Seq[Bool], outs: Seq[ClientUncachedTileLinkIO], ins: Seq[ClientUncachedTileLinkIO]) {
    for ((sel, out, in) <- (sels, outs, ins).zipped) {
      connectWhen(sel, out, in)
    }
  }

  def disconnectIn(ins: Seq[ClientUncachedTileLinkIO]) {
    for (in <- ins) { disconnectIn(in) }
  }

  def disconnectOut(outs: Seq[ClientUncachedTileLinkIO]) {
    for (out <- outs) { disconnectOut(out) }
  }

  disconnectIn(io.in)

  for ((out, i) <- io.out.zipWithIndex) {
    val selects = allowedRoutes(i).map(j => io.select(j) === UInt(i))
    val inputs = allowedRoutes(i).map(io.in(_))
    val arb = Module(new ClientUncachedTileLinkIOArbiter(inputs.size))
    disconnectOut(arb.io.in)
    connectWhen(selects, arb.io.in, inputs)
    out <> arb.io.out
  }
}
