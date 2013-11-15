package spray.ext
package directives
package magnets

import shapeless.{HNil, ::}
import spray.httpx.marshalling.ToResponseMarshaller
import spray.routing._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}
import scala.util.control.NonFatal
import scalaz.{OptionT, EitherT, \/}
import scalaz.concurrent.Task
import scalaz.syntax.either._

trait ExecutionMagnets {
  def withResult[T](magnet: WithResultMagnet[T]): D1[Throwable \/ T] = magnet
  def withSuccess[T](magnet: WithSuccessMagnet[T]): D1[T] = magnet
  def withFailed(magnet: WithFailedMagnet): D1[Throwable] = magnet
}
object ExecutionMagnets extends ExecutionMagnets

/****************************************************************************************
 * MAGNET IMPLEMENTATIONS
 ****************************************************************************************/

trait WithResultMagnet[T] extends D1[Throwable \/ T]
object WithResultMagnet {
  private def hl[T](value: T): T :: HNil = value :: HNil

  implicit def apply[T](task: Task[T]) = new WithResultMagnet[T] {
    def happly(f: ((Throwable \/ T) :: HNil) => Route): Route = { ctx =>
      try task.runAsync(e => f(hl(e))(ctx))
      catch { case NonFatal(error) ⇒ ctx.failWith(error) }
    }
  }

  implicit def apply[T](future: Future[T])(implicit ec: ExecutionContext) = new WithResultMagnet[T] {
    def happly(f: ((Throwable \/ T) :: HNil) => Route): Route = ctx => future onComplete {
      case Failure(error) => ctx failWith error
      case Success(result) => try f(hl(result.right))(ctx) catch {
        case NonFatal(error) => ctx failWith error
      }
    }
  }

  implicit def apply[T](either: Throwable \/ T) = new WithResultMagnet[T] {
    def happly(f: ((Throwable \/ T) :: HNil) => Route): Route = f(hl(either))
  }
}

trait WithSuccessMagnet[T] extends D1[T]
object WithSuccessMagnet {
  private def hl[T](value: T): T :: HNil = value :: HNil
  private def handle[T](action: ((Throwable \/ T) => Unit) => Unit)(f: (T :: HNil) => Route)(ctx: RequestContext) = {
    action(_.fold(ctx.failWith, { result =>
      try f(hl(result))(ctx)
      catch { case NonFatal(error) ⇒ ctx.failWith(error) }
    }))
  }

  implicit def apply[T](task: Task[T]): WithSuccessMagnet[T] = {
    new WithSuccessMagnet[T] {
      def happly(f: (T :: HNil) => Route): Route = handle(task.runAsync)(f)
    }
  }

  implicit def apply[T](future: Future[T])(implicit ec: ExecutionContext): WithSuccessMagnet[T] = {
    new WithSuccessMagnet[T] {
      def happly(f: (T :: HNil) => Route): Route = { ctx =>
        future.onSuccess { case result =>
          try f(result :: HNil)(ctx)
          catch { case NonFatal(error) ⇒ ctx.failWith(error) }
        }
      }
    }
  }

  implicit def apply[T](either: Throwable \/ T): WithSuccessMagnet[T] = {
    new WithSuccessMagnet[T] {
      def happly(f: (T :: HNil) => Route): Route = { ctx =>
        either.fold(ctx.failWith, r => f(hl(r))(ctx))
      }
    }
  }
}

trait WithFailedMagnet extends D1[Throwable]
object WithFailedMagnet {
  implicit def apply[T: ToResponseMarshaller](task: Task[T]) = {
    new WithFailedMagnet {
      def happly(f: (Throwable :: HNil) => Route): Route = { ctx =>
        task runAsync { _.fold({ error =>
          try f(error :: HNil)(ctx)
          catch { case NonFatal(err) ⇒ ctx.failWith(err) }
        }, ctx.complete(_)) }
      }
    }
  }

//  implicit def apply[T](future: Future[T])(implicit ec: ExecutionContext, trm: ToResponseMarshaller[T]) = {
//    new WithFailedMagnet {
//      def happly(f: (Throwable :: HNil) => Route): Route = { ctx =>
//        future.unsafeFoldComplete(e => f(e :: HNil)(ctx))(ctx.complete(_))
//      }
//    }
//  }

  implicit def apply[T: ToResponseMarshaller](either: Throwable \/ T) = {
    new WithFailedMagnet {
      override def happly(f: (Throwable :: HNil) => Route): Route = ctx =>
        either.fold(ex => f(ex :: HNil)(ctx), ctx.complete(_))
    }
  }

}