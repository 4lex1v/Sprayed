package spray.ext

import spray.routing.{Directive1, Directive0}

package object directives {
  type D1[A]  = Directive1[A]
  type D0     = Directive0
  type D0C[A] = Directive0

  def toDirective[M[_]: AsDirective, A](fromContext: M[A]) =
    AsDirective[M].toDirective(fromContext)

}