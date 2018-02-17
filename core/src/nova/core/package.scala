package nova

package object core extends static.Printing with static.Scoping with static.Staging with static.Typing {
  type SrcCtx = forge.SrcCtx

  def syms(a: Any*): Seq[Sym[_]] = core.Filters.syms(a:_*)
}