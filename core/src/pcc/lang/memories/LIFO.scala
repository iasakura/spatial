package pcc.lang
package memories

import forge.api
import pcc.core._
import pcc.node._

import scala.collection.mutable

case class LIFO[A:Bits]() extends LocalMem[A,LIFO] {
  override type I = mutable.MutableList[AI]
  override def fresh: LIFO[A] = new LIFO[A]
}
object LIFO {
  private lazy val types = new mutable.HashMap[Bits[_],LIFO[_]]()
  implicit def tp[A:Bits]: LIFO[A] = types.getOrElseUpdate(bits[A], (new LIFO[A]).asType).asInstanceOf[LIFO[A]]

  @api def apply[A:Bits](depth: I32): LIFO[A] = stage(LIFONew(depth))
}
