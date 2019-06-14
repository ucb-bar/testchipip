package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.util.Random

class ShuffleTable(bitIndices: Seq[Int], remapping: Seq[Int]) {
  val nBits = bitIndices.size
  lazy val tableVec = VecInit(remapping.map(_.U(nBits.W)))

  def shuffle(addr: UInt): UInt = {
    // Table validation, bitIndices.size should be log2 of remapping.size
    // and table entries should be unique
    require(nBits == log2Ceil(remapping.size))
    remapping.sorted.zipWithIndex.foreach { case (j, i) => require(j == i) }

    val leftIndices = -1 +: bitIndices.init
    val rightIndices = bitIndices
    val lastIdx = bitIndices.last

    val toShuffle = Cat(bitIndices.map(idx => addr(idx)).reverse)
    val shuffled = tableVec(toShuffle).toBools
    val unshuffled = (leftIndices zip rightIndices).map {
      case (lidx, ridx) => addr(ridx-1, lidx+1)
    }

    val pieces = (shuffled.zip(unshuffled).map { case (shuf, unshuf) =>
      Cat(shuf, unshuf)
    }) :+ (addr >> (lastIdx + 1).U)

    Cat(pieces.reverse)
  }

  def reverse(): ShuffleTable =
    new ShuffleTable(bitIndices, ShuffleTable.reverseTable(remapping))
}

object ShuffleTable {
  def reverseTable(mapping: Seq[Int]): Seq[Int] = {
    val reverseMapping = ArrayBuffer.fill(mapping.size)(0)
    mapping.zipWithIndex.foreach { case (j, i) =>
      reverseMapping(j) = i
    }
    reverseMapping.toSeq
  }

  def random(ranges: Seq[(Int, Int)]): ShuffleTable = {
    val bitIndices = ranges.map { case (start, end) =>
      start + Random.nextInt(end - start)
    }
    val mapping = randomMapping(bitIndices.size)

    // Print these out for auditing purposes
    println(s"bit indices: $bitIndices")
    println(s"mapping: $mapping")

    new ShuffleTable(bitIndices, mapping)
  }

  def randomMapping(sbits: Int): Seq[Int] =
    Random.shuffle(Seq.tabulate(1 << sbits)(i => i))
}

/**
 * Reversably shuffle bits [msb-1:lsb] of addresses according to a shuffle table
 * Each segBits-sized piece of the address gets mapped to a distinct integer
 */
class TLAddressShuffler(table: ShuffleTable)
    (implicit p: Parameters) extends LazyModule {

  def this(ranges: Seq[(Int, Int)])(implicit p: Parameters) =
    this(ShuffleTable.random(ranges))

  val node = TLAdapterNode(
    clientFn = { cp => cp },
    managerFn = { mp => mp })

  val reverseTable = table.reverse()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val bce = edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe

      out <> in
      out.a.bits.address := table.shuffle(in.a.bits.address)

      if (bce) {
        in.b.bits.address := reverseTable.shuffle(out.b.bits.address)
        out.c.bits.address := table.shuffle(in.c.bits.address)
      }
    }
  }
}

object TLAddressShuffler {
  def apply(table: ShuffleTable)(implicit p: Parameters): TLNode = {
    val shuffler = LazyModule(new TLAddressShuffler(table))
    shuffler.node
  }

  def apply(ranges: Seq[(Int, Int)])(implicit p: Parameters): TLNode =
    apply(ShuffleTable.random(ranges))
}
