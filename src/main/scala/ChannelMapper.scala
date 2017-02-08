package testchipip

import chisel3._
import chisel3.util._
import uncore.tilelink._
import cde.Parameters

/**
 * Assumes every channel has distinct addresses strided per block.
 * Cuts down the addresses on the output so that all the addresses for each
 * channel are contiguous.
 */
class ChannelAddressMapper(n: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Vec(n, new ClientUncachedTileLinkIO))
    val out = Vec(n, new ClientUncachedTileLinkIO)
  })

  if (n <= 1) {
    io.out <> io.in
  } else {
    val idBits = log2Up(n)

    for (((in, out), i) <- io.in.zip(io.out).zipWithIndex) {
      val in_addr_block = in.acquire.bits.addr_block
      out.acquire.valid := in.acquire.valid
      in.acquire.ready := out.acquire.ready
      out.acquire.bits := in.acquire.bits
      out.acquire.bits.addr_block := in_addr_block >> idBits.U
      in.grant <> out.grant
    }
  }
}

/**
 * Does the opposite of the mapper. Takes contiguous addresses on the input
 * and transforms them to be distinct per channel on the output.
 */
class ChannelAddressUnmapper(n: Int, c: Clock = null, r: Bool = null)(implicit p: Parameters)
    extends Module(Option(c), Option(r)) {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new ClientUncachedTileLinkIO))
    val out = Vec(n, new ClientUncachedTileLinkIO)
  })

  val idBits = log2Up(n)

  if (n <= 1) {
    io.out <> io.in
  } else {
    for (((in, out), i) <- io.in.zip(io.out).zipWithIndex) {
      val in_addr_block = in.acquire.bits.addr_block
      out.acquire.valid := in.acquire.valid
      in.acquire.ready := out.acquire.ready
      out.acquire.bits := in.acquire.bits
      out.acquire.bits.addr_block := Cat(in_addr_block, UInt(i, idBits))
      in.grant <> out.grant
    }
  }
}

object ChannelAddressMapper {
  def map(in: Seq[ClientUncachedTileLinkIO])(implicit p: Parameters): Seq[ClientUncachedTileLinkIO] = {
    val mapper = Module(new ChannelAddressMapper(in.size))
    mapper.io.in <> in
    mapper.io.out
  }

  def unmap(in: Seq[ClientUncachedTileLinkIO])(implicit p: Parameters): Seq[ClientUncachedTileLinkIO] = {
    val unmapper = Module(new ChannelAddressUnmapper(in.size))
    unmapper.io.in <> in
    unmapper.io.out
  }
}
