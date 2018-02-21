package nova.data

import forge.tags._
import core._

import spatial.lang._

sealed abstract class ControlLevel
case object Outer extends ControlLevel
case object Inner extends ControlLevel

case class Ctrl(sym: Sym[_], id: Int)

case class CtrlLevel(level: ControlLevel) extends StableData[CtrlLevel]
@data object levelOf {
  def get(x: Sym[_]): Option[ControlLevel] = metadata[CtrlLevel](x).map(_.level)
  def apply(x: Sym[_]): ControlLevel = levelOf.get(x).getOrElse{throw new Exception(s"Undefined control level for $x") }
  def update(x: Sym[_], level: ControlLevel): Unit = metadata.add(x, CtrlLevel(level))
}
@data object isOuter {
  def apply(x: Sym[_]): Boolean = levelOf(x) == Outer
  def update(x: Sym[_], isOut: Boolean): Unit = if (isOut) levelOf(x) = Outer else levelOf(x) = Inner
}

case class Children(children: Seq[Sym[_]]) extends FlowData[Children]
@data object childrenOf {
  def apply(x: Sym[_]): Seq[Sym[_]] = metadata[Children](x).map(_.children).getOrElse(Nil)
  def update(x: Sym[_], children: Seq[Sym[_]]): Unit = metadata.add(x, Children(children))
}

case class Parent(parent: Ctrl) extends FlowData[Parent]
@data object parentOf {
  def get(x: Sym[_]): Option[Ctrl] = metadata[Parent](x).map(_.parent)
  def apply(x: Sym[_]): Ctrl = parentOf.get(x).getOrElse{throw new Exception(s"Undefined parent for $x") }
  def update(x: Sym[_], parent: Ctrl): Unit = metadata.add(x, Parent(parent))
}

case class IndexCounter(ctr: Counter) extends FlowData[IndexCounter]
@data object ctrOf {
  def get(i: I32): Option[Counter] = metadata[IndexCounter](i).map(_.ctr)
  def apply(i: I32): Counter = ctrOf.get(i).getOrElse{throw new Exception(s"No counter associated with $i") }
  def update(i: I32, ctr: Counter): Unit = metadata.add(i, IndexCounter(ctr))
}