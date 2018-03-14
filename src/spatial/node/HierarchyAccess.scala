package spatial.node

import core._
import forge.tags._
import spatial.lang._

/** Memory accesses */
abstract class Access {
  def mem:  Sym[_]
  def addr: Seq[Idx]
  def ens:  Set[Bit]
}
case class Read(mem: Sym[_], addr: Seq[Idx], ens: Set[Bit]) extends Access
case class Write(mem: Sym[_], data: Sym[_], addr: Seq[Idx], ens: Set[Bit]) extends Access

abstract class BankedAccess {
  def mem:  Sym[_]
  def bank: Seq[Seq[Idx]]
  def ofs:  Seq[Idx]
  def ens:  Seq[Set[Bit]]
}
case class BankedRead(mem: Sym[_], bank: Seq[Seq[Idx]], ofs: Seq[Idx], ens: Seq[Set[Bit]]) extends BankedAccess
case class BankedWrite(mem: Sym[_], data: Seq[Sym[_]], bank: Seq[Seq[Idx]], ofs: Seq[Idx], ens: Seq[Set[Bit]]) extends BankedAccess


/** Status read of a memory */
abstract class StatusRead[R:Type] extends EnPrimitive[R] {
  def mem: Sym[_]
}
object StatusRead {
  def unapply(x: Op[_]): Option[(Sym[_],Set[Bit])] = x match {
    case a: StatusRead[_] => Some((a.mem,a.ens))
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Sym[_],Set[Bit])] = x.op.flatMap(StatusRead.unapply)
}

/** Reset of a memory */
abstract class Resetter[A:Type] extends EnPrimitive[Void] {
  val tA: Type[A] = Type[A]
  def mem: Sym[_]
}
object Resetter {
  def unapply(x: Op[_]): Option[(Sym[_],Set[Bit])] = x match {
    case a: Resetter[_] => Some((a.mem,a.ens))
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Sym[_],Set[Bit])] = x.op.flatMap(Resetter.unapply)
}




/** Any access of a memory */
abstract class Accessor[A:Bits,R:Type] extends EnPrimitive[R] {
  val A: Bits[A] = Bits[A]
  def mem:  Sym[_]
  def addr: Seq[Idx]
  def dataOpt: Option[Sym[_]] = localWrite.map(_.data)
  def localRead: Option[Read]
  def localWrite: Option[Write]
  def localAccesses: Set[Access] = (localRead ++ localWrite).toSet
}

object Accessor {
  def unapply(x: Op[_]): Option[(Option[Write],Option[Read])] = x match {
    case a: Accessor[_,_] if a.localWrite.nonEmpty || a.localRead.nonEmpty =>
      Some((a.localWrite,a.localRead))
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Option[Write],Option[Read])] = x.op.flatMap(Accessor.unapply)
}

/** Any read of a memory */
abstract class Reader[A:Bits,R:Bits] extends Accessor[A,R] {
  def localRead = Some(Read(mem,addr,ens))
  def localWrite: Option[Write] = None
}

object Reader {
  def unapply(x: Op[_]): Option[(Sym[_],Seq[Idx],Set[Bit])] = x match {
    case a: Accessor[_,_] => a.localRead.map{rd => (rd.mem,rd.addr,rd.ens) }
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Sym[_],Seq[Idx],Set[Bit])] = x.op.flatMap(Reader.unapply)
}

/** Any dequeue-like operation from a memory */
abstract class DequeuerLike[A:Bits,R:Bits] extends Reader[A,R]

/** An address-less dequeue operation. */
abstract class Dequeuer[A:Bits,R:Bits] extends DequeuerLike[A,R] {
  def addr: Seq[Idx] = Nil
}

object Dequeuer {
  def unapply(x: Op[_]): Option[(Sym[_],Seq[Idx],Set[Bit])] = x match {
    case a: Dequeuer[_,_] => a.localRead.map{rd => (rd.mem,rd.addr,rd.ens) }
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Sym[_],Seq[Idx],Set[Bit])] = x.op.flatMap(Dequeuer.unapply)
}


/** Any write to a memory */
abstract class Writer[A:Bits] extends Accessor[A,Void] {
  override def effects: Effects = Effects.Writes(mem)

  def data: Sym[_]
  def localRead: Option[Read] = None
  def localWrite = Some(Write(mem,data,addr,ens))
}

object Writer {
  def unapply(x: Op[_]): Option[(Sym[_],Sym[_],Seq[Idx],Set[Bit])] = x match {
    case a: Accessor[_,_] => a.localWrite.map{wr => (wr.mem,wr.data,wr.addr,wr.ens) }
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Sym[_],Sym[_],Seq[Idx],Set[Bit])] = x.op.flatMap(Writer.unapply)
}

/** Any enqueue-like operation to a memory */
abstract class EnqueuerLike[A:Bits] extends Writer[A]

/** An address-less enqueue operation. */
abstract class Enqueuer[A:Bits] extends Writer[A] {
  def addr: Seq[Idx] = Nil
}

object Enqueuer {
  def unapply(x: Op[_]): Option[(Sym[_],Sym[_],Seq[Idx],Set[Bit])] = x match {
    case a: Enqueuer[_] => a.localWrite.map{wr => (wr.mem,wr.data,wr.addr,wr.ens) }
    case _ => None
  }
  def unapply(x: Sym[_]): Option[(Sym[_],Sym[_],Seq[Idx],Set[Bit])] = x.op.flatMap(Enqueuer.unapply)
}







