package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.util.Random

class ShuffleTable(rows: Seq[Seq[Int]]) {
  lazy val rowVecs = rows.map(
    row => VecInit(row.map(_.U(log2Ceil(row.size).W))))

  def shuffle(addr: UInt, lsb: Int = 0): UInt = {
    // table validation: make sure each entry in a row is unique
    for (row <- rows) {
      row.sorted.zipWithIndex.foreach { case (i, j) => require(i == j) }
    }
    val (msb, pieces) = rowVecs.foldLeft((lsb, List.empty[UInt])) {
      case ((base, lst), row) =>
        val sbits = log2Ceil(row.size)
        val addrPart = addr(sbits + base - 1, base)
        (base + sbits, row(addrPart) :: lst)
    }
    Cat(addr >> msb.U, Cat(pieces), addr(lsb-1, 0))
  }

  def reverse(): ShuffleTable =
    new ShuffleTable(ShuffleTable.reverseTable(rows))
}

object ShuffleTable {
  def reverseRow(row: Seq[Int]): Seq[Int] = {
    val reverseRow = ArrayBuffer.fill(row.size)(0)
    row.zipWithIndex.foreach { case (j, i) =>
      reverseRow(j) = i
    }
    reverseRow.toSeq
  }

  def reverseTable(table: Seq[Seq[Int]]): Seq[Seq[Int]] =
    table.map(row => reverseRow(row))

  def random(addrBits: Int, segBits: Int): ShuffleTable = {
    val lengths = ListBuffer.fill(addrBits/segBits)(segBits)
    
    if (addrBits % segBits > 0)
      lengths += (addrBits % segBits)

    random(lengths.toSeq:_*)
  }

  def random(segBits: Int*): ShuffleTable = {
    val rows = segBits.map(sbits =>
      Random.shuffle(Seq.tabulate(1 << sbits)(i => i)))
    new ShuffleTable(rows)
  }
}

/**
 * Reversably shuffle bits [msb-1:lsb] of addresses according to a shuffle table
 * Each segBits-sized piece of the address gets mapped to a distinct integer
 */
class TLAddressShuffler(msb: Int, lsb: Int,
    segBits: Int = 3, tableOverride: Seq[Seq[Int]] = Nil)
    (implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode(
    clientFn = { cp => cp },
    managerFn = { mp => mp })

  val table = if (tableOverride.isEmpty) {
    ShuffleTable.random(msb-lsb, segBits)
  } else {
    // tableOverride validation: make sure bits add up
    val totalBits = tableOverride.map(row => log2Ceil(row.size)).reduce(_ + _)
    require(totalBits == (msb-lsb))

    new ShuffleTable(tableOverride)
  }
  val reverseTable = table.reverse()

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val bce = edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe

      out <> in
      out.a.bits.address := table.shuffle(in.a.bits.address, lsb)

      if (bce) {
        in.b.bits.address := reverseTable.shuffle(out.b.bits.address, lsb)
        out.c.bits.address := table.shuffle(in.c.bits.address, lsb)
      }
    }
  }
}

object TLAddressShuffler {
  def apply(msb: Int, lsb: Int, segBits: Int = 3, tableOverride: Seq[Seq[Int]] = Nil)
      (implicit p: Parameters) = {
    val shuffler = LazyModule(new TLAddressShuffler(
      msb, lsb, segBits, tableOverride))
    shuffler.node
  }
}
