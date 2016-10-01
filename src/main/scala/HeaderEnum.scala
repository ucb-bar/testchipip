package testchipip

import Chisel._
import scala.collection.mutable.HashMap

class HeaderEnum(val prefix: String) {
  val h = new HashMap[String,Int]
  def makeHeader(): String = {
    h.toSeq.sortBy(_._2).map { case (n,i) => s"#define ${prefix.toUpperCase}_${n.toUpperCase} $i\n" } mkString
  }
  def apply(s: String): UInt = UInt(h(s), width=log2Up(h.size))
}

object HeaderEnum {
  def apply(prefix: String, names: String*): HeaderEnum = {
    val e = new HeaderEnum(prefix)
    names.zipWithIndex.foreach { case (n,i) => e.h.put(n,i) }
    SCRHeaderOutput.add(e.makeHeader())
    e
  }
}
