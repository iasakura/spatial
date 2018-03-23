package nova.compiler

import core._
import core.passes.Pass
import spatial.data.FlowRules
import spatial.rewrites.RewriteRules
import spatial.lang.Void

trait DSLApp extends Compiler {
  val script = "nova"
  val desc = "Nova compiler"
  new RewriteRules {}
  new FlowRules {}

  protected implicit class BlockOps[R](block: Block[R]) {
    def ==>(pass: Pass): Block[R] = runPass(pass, block)
    def ==>(pass: (Boolean,Pass)): Block[R] = if (pass._1) runPass(pass._2,block) else block
  }
  protected implicit class ConditionalPass(cond: Boolean) {
    def ?(pass: Pass): (Boolean, Pass) = (cond, pass)
  }

  def main(): Void

  final def stage(args: Array[String]): Block[_] = {
    val block = stageBlock{ main() }
    block
  }
}