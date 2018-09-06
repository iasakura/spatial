package spatial.codegen.dotgen

import argon._
import spatial.metadata.control._

class HtmlIRGenSpatial(val IR: State) extends HtmlIRCodegen {

  override def entryFile: String = s"IR.$ext"
  override def clearGen(): Unit = {}

  override protected def quoteConst(tp: Type[_], c: Any): String = s"$c"

  override protected def quote(s: Sym[_]): String = s.rhs match {
    case (_:Def.Node[_] | _:Def.Bound[_]) => 
      val q = super.quote(s)
      elem("a", q, "href"->s"file:///$out/IR.html#$q")
    case _ => super.quote(s)
  }

  override protected def quoteOrRemap(arg: Any): String = arg match {
    case s: String     => s
    case c: Int        => c.toString
    case b: Boolean    => b.toString
    case l: Long       => l.toString + "L"
    case d: Double     => d.toString
    case l: BigDecimal => l.toString
    case l: BigInt     => l.toString
    case c: SrcCtx     => c.toString
    case p: Ref[_,_]   => quote(p)

    case p: Effects => p.toString
    case p: spatial.metadata.control.Blk => src"Blk(s=${p.s}, block=${p.block})"
    case p: spatial.metadata.control.Scope => src"Scope(s=${p.s}, stage=${p.stage}, block=${p.block})"
    case p: spatial.metadata.control.Ctrl => src"Ctrl(s=${p.s}, stage=${p.stage})"

    case p:Tuple2[_,_] => s"(${p.productIterator.map(quoteOrRemap).mkString(",")})"
    case p:Tuple3[_,_,_] => s"(${p.productIterator.map(quoteOrRemap).mkString(",")})"
    case p:Tuple4[_,_,_,_] => s"(${p.productIterator.map(quoteOrRemap).mkString(",")})"
    case p:Tuple5[_,_,_,_,_] => s"(${p.productIterator.map(quoteOrRemap).mkString(",")})"
    case None    => "None"
    case Some(x) => "Some(" + quoteOrRemap(x) + ")"
    case p:Map[_,_] =>
      s"{${p.map { case (k,v) => src"""$k:$v"""}.mkString(",")}}"
    case p: Iterable[_] => 
      s"[${p.map(quoteOrRemap).mkString(", ")}]" 
    case p: Product => 
      val fields = p.getClass.getDeclaredFields.map(_.getName)
      val values = p.productIterator.toArray
      val fs = fields.zip(values).map { case (k,v) => src"$k=$v"}.mkString(",")
      s"${p.productPrefix}($fs)"

    case _ => arg.toString
  }

  override def emitMeta(lhs: Sym[_]): Unit = lhs match {
    case lhs if lhs.blocks.nonEmpty =>
      emit(elem("a", "dot<br>", "href"->s"file:///$out/$lhs.html"))
      super.emitMeta(lhs)
    case lhs =>
      val parent = lhs match {
        case lhs if lhs.isBound => lhs.parent.s
        case _ => lhs.blk.s
      }
      val parentFile = parent.map { sym => s"$sym" }.getOrElse("Top")
      emit(elem("a", "dot<br>", "href"->s"file:///$out/$parentFile.html"))
      super.emitMeta(lhs)
  }

  override def emitMeta(data:Data[_]) = data match {
    case spatial.metadata.memory.Duplicates(d) =>
      text(src"${elem("strong",data.getClass.getSimpleName)}")
      emitElem("ul", "style"->"list-style-type:disc") {
        d.foreach { case spatial.metadata.memory.Memory(banking, depth, padding, accType) =>
          emitElem("li"){
            text(src"${elem("strong","banking")}")
            emitElem("ul", "style"->"list-style-type:none") {
              banking.foreach { data =>
                emitElem("li", src"${data.toString}")
              }
            }
            text(src"${elem("strong","depth")}: $depth")
            text(src"${elem("strong","padding")}: $padding")
            text(src"${elem("strong","accType")}: $accType")
          }
        }
      }
    case data => super.emitMeta(data)
  }

}
object HtmlIRGenSpatial {
  def apply(IR:State) = new HtmlIRGenSpatial(IR)
}

