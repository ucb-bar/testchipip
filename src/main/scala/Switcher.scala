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
}

/** Route the input interfaces to the output interfaces
 *  based on the settings in select.
 *  If bit i of element j of select is high,
 *  bank i will be routed to channel j.
 *  It is not safe to change select when the selector is taking traffic.
 *  Only certain configurations are valid.
 *  Each bank must be routed to exactly one channel. */
class ClientTileLinkIOSwitcher(nBanks: Int, nMemChannels: Int, clockSignal: Clock = null, resetSignal: Bool = null)
                              (implicit p: Parameters)
                              extends TLModule(clockSignal, resetSignal)(p)
                              with SwitchesTileLinkChannels {
  val io = new Bundle {
    val select = Vec(nBanks, UInt(INPUT, log2Up(nMemChannels)))
    val in = Vec(nBanks, new ClientTileLinkIO).flip
    val out = Vec(nMemChannels, new ClientTileLinkIO)
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
    val selects = Seq.tabulate(nBanks)(j => io.select(j) === UInt(i))
    val arb = Module(new ClientTileLinkIOArbiter(nBanks))
    disconnectOut(arb.io.in)
    connectWhen(selects, arb.io.in, io.in)
    out <> arb.io.out
  }
}

class ClientUncachedTileLinkIOSwitcher(nBanks: Int, nMemChannels: Int, clockSignal: Clock = null, resetSignal: Bool = null)
                                      (implicit p: Parameters)
                                      extends TLModule(clockSignal, resetSignal)(p)
                                      with SwitchesTileLinkChannels {
  val io = new Bundle {
    val select = Vec(nBanks, UInt(INPUT, log2Up(nMemChannels)))
    val in = Vec(nBanks, new ClientUncachedTileLinkIO).flip
    val out = Vec(nMemChannels, new ClientUncachedTileLinkIO)
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
    val selects = Seq.tabulate(nBanks)(j => io.select(j) === UInt(i))
    val arb = Module(new ClientUncachedTileLinkIOArbiter(nBanks))
    disconnectOut(arb.io.in)
    connectWhen(selects, arb.io.in, io.in)
    out <> arb.io.out
  }
}
