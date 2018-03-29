package spatial.rewrites

import argon._
import forge.tags._
import spatial.lang._
import spatial.node._
import spatial.util._

trait AliasRewrites extends RewriteRules {

  @rig def combineSeries(series1: Seq[Series[Idx]], series2: Seq[Series[Idx]]): Seq[Series[Idx]] = {
    def _combineSeries[A<:Exp[_,A]:IntLike](r1: Series[A], r2: Series[A]): Series[Idx] = {
      import IntLike._

      val start = r1.start + r2.start*r1.step
      implicit val num: Num[Idx] = start.asInstanceOf[Idx].tp.asInstanceOf[Num[Idx]]
      val end   = min[Idx](r1.end.asInstanceOf[Idx], (r1.start + r2.end*r1.step).asInstanceOf[Idx])
      val step  = r1.step * r2.step
      val par   = (r1.par, r2.par) match {
        case (Literal(1), p) => p
        case (p, Literal(1)) => p
        case _ => r1.par
      }
      Series[Idx](start = start.asInstanceOf[Idx], end = end.asInstanceOf[Idx], step = step.asInstanceOf[Idx], par = par)
    }

    series2.zip(series1).map{case (r2, r1) => _combineSeries(r1, r2) }
  }

  @rig def rewriteDenseAlias[A,Src[T],Alias[T]](
    op: MemDenseAlias[A,Src,Alias]
  )(implicit
    A:     Type[A],
    Src:   Type[Src[A]],
    Alias: Type[Alias[A]]
  ): Sym[_] = {
    val MemDenseAlias(cond, mem, series) = op
    dbgs(s"Checking rewrite rule for mem alias:")
    dbgs(s"$op")
    val mems = mem.map{mem => box(mem) }

    if (mems.exists{_.isDenseAlias}) {
      val aliases = (cond, mems, series).zipped.flatMap{
        case (c2, Op(MemDenseAlias(conds1, mems1, seriess1)), series2) =>
          (conds1, mems1, seriess1).zipped.map{
            case (c1, mem1, series1) =>
              (c1 & c2, mem1.asInstanceOf[Src[A]], combineSeries(series1, series2))
          }

        case (cond2, mem2, series2) =>
          Seq((cond2, mem2.unbox, series2))
      }
      val conds3   = aliases.map(_._1)
      val mems3    = aliases.map(_._2)
      val seriess3 = aliases.map(_._3)
      stage(MemDenseAlias[A,Src,Alias](conds3, mems3, seriess3))
    }
    else Invalid
  }

  @rewrite def nested_alias(op: MemDenseAlias[_,C forSome{type C[_]},A forSome{type A[_]}]): Sym[_] = {
    case op: MemDenseAlias[_,_,_] => rewriteDenseAlias(op)(op.A,op.Src,op.Alias,ctx,state)
  }

}