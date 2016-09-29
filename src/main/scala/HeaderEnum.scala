package testchipip

import Chisel._
import scala.collection.mutable.HashMap

class HeaderEnum {
  val h = new HashMap[String,Int]
  def makeHeader(prefix: String): String = {
    h.toSeq.sortBy(_._2).map { case (n,i) => s"#define ${prefix}_$n $i\n" } mkString
  }
  def apply(s: String): UInt = UInt(h(s), width=log2Up(h.size))
}

object HeaderEnum {
  def apply(names: String*): HeaderEnum = {
    val e = new HeaderEnum
    names.zipWithIndex.foreach { case (n,i) => e.h.put(n,i) }
    e
  }
}
