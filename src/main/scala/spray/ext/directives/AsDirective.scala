package spray.ext
package directives

import shapeless.{HNil, ::}

import scala.concurrent.Future

import spray.routing.Directives._
import spray.routing.{Route, Directive, HListable}
import spray.routing.directives.OnSuccessFutureMagnet

import scala.concurrent.ExecutionContext
import scalaz.{\/, -\/, \/-}
import scalaz.concurrent.{Task, Future => FutureZ}

trait AsDirective[M[_]] {
  def toDirective[A](context: M[A]): D1[A]
}

object AsDirective {
  def apply[M[_]: AsDirective] = implicitly[AsDirective[M]]

  // Scala Future
  implicit def forFuture(implicit ctx: ExecutionContext): AsDirective[Future] =
    new AsDirective[Future] {
      override def toDirective[A](context: Future[A]): D1[A] =
        onSuccess(context) flatMap {
          case result => provide(result)
        }
    }

  // Scalaz Future
  implicit def forFutureZ: AsDirective[FutureZ] =
    new AsDirective[FutureZ] {
      override def toDirective[A](context: FutureZ[A]): D1[A] = new D1[A]{
        override def happly(f: (A :: HNil) => Route): Route = { ctx =>
          try context.runAsync(a => f(a :: HNil)(ctx)) catch {
            case e: Exception => ctx.failWith(e)
          }
        }
      }
    }

  implicit def forTask: AsDirective[Task] =
    new AsDirective[Task] {
      override def toDirective[A](context: Task[A]): D1[A] = new D1[A] {
        override def happly(f: (A :: HNil) => Route): Route = { ctx =>
          try context runAsync {
            case -\/(error) => ctx.failWith(error)
            case \/-(result) => f(result :: HNil)(ctx)
          } catch {
            case e: Exception => ctx.failWith(e)
          }
        }
      }
    }

  // Convert Throwable \/ A to Directive1[A], rejecting Throwable
  implicit def forDisjunction: AsDirective[({type λ[α] = \/[Throwable, α]})#λ] =
    new AsDirective[({ type λ[α] = Throwable \/ α })#λ] {
      override def toDirective[A](context: Throwable \/ A): D1[A] = new D1[A] {
        override def happly(f: (A :: HNil) => Route): Route = { ctx =>
          context.fold(ctx.failWith, a => f(a :: HNil)(ctx))
        }
      }
    }

}