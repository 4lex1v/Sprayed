package spray.ext
package marshallers

import akka.actor.ActorRefFactory
import spray.http.HttpEntity
import spray.httpx.marshalling.{MarshallingContext, Marshaller, MarshallerM}
import spray.routing.{RejectionError, Rejection}

import scalaz.Id.Id
import scalaz.{StreamT, Monad}
import scalaz.effect.IO

trait MiscMarshallers {
  implicit def idMarshaller[A: Marshaller] =
    new Marshaller[Id[A]] {
      override def apply(value: A, ctx: MarshallingContext): Unit =
        implicitly[Marshaller[A]].apply(value, ctx)
    }

  implicit val unitMarshaller: Marshaller[Unit] =
    new Marshaller[Unit] {
      def apply(value: Unit, ctx: MarshallingContext): Unit = {
        ctx.marshalTo(HttpEntity.Empty)
      }
    }

  implicit val rejectionMarshaller: Marshaller[Rejection] =
    Marshaller[Rejection] { (value, ctx) =>
      ctx.handleError(RejectionError(value))
    }

  implicit val throwableMarshaller: Marshaller[Throwable] =
    Marshaller[Throwable] { (value, ctx) =>
      ctx.handleError(value)
    }

  implicit val ioMarshaller: MarshallerM[IO] = new MarshallerM[IO] {
    def marshaller[T](implicit tm: Marshaller[T]) = new Marshaller[IO[T]] {
      def apply(value: IO[T], ctx: MarshallingContext): Unit = {
        value.catchLeft.unsafePerformIO().fold(ctx.handleError, tm(_, ctx))
      }
    }
  }

  implicit def streamTMarshaller[M[_]](implicit mtm: MarshallerM[M], M: Monad[M], arf: ActorRefFactory): MarshallerM[({ type λ[α] = StreamT[M, α] })#λ] = {
    new MarshallerM[({ type λ[α] = StreamT[M, α] })#λ] {
      override def marshaller[T](implicit mt: Marshaller[T]): Marshaller[StreamT[M, T]] = {
        new Marshaller[StreamT[M, T]] {
          override def apply(value: StreamT[M, T], ctx: MarshallingContext): Unit = {
            mtm.marshaller[Stream[T]].apply(value.toStream, ctx)
          }
        }
      }
    }
  }

}

object MiscMarshallers extends MiscMarshallers