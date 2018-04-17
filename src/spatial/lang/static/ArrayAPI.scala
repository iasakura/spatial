package spatial.lang
package static

import argon._
import forge.tags._

trait ArrayAPI {

  implicit class NestedTensorInfixOps[A](a: Tensor1[Tensor1[A]]) {
    private implicit val A: Type[A] = a.A.unbox.A

    @api def flatten: Tensor1[A] = a.flatMap{x => x}
  }

}
