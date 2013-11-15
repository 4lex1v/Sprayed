package spray.ext
package marshallers

import spray.httpx.marshalling.{MarshallingContext, Marshaller, MarshallerM}

import scalaz.concurrent.{Task, Future}

trait ConcurrentMarshallers {

  implicit val taskMarshaller: MarshallerM[Task] =
    new MarshallerM[Task] {
      def marshaller[T](implicit tm: Marshaller[T]) = {
        new Marshaller[Task[T]] {
          def apply(value: Task[T], ctx: MarshallingContext): Unit = {
            value runAsync { _.fold(ctx.handleError, tm(_, ctx)) }
          }
        }
      }
    }

  // for Scalaz Future
  implicit val futureMarshaller: MarshallerM[Future] =
    new MarshallerM[Future] {
      override def marshaller[T](implicit tm: Marshaller[T]): Marshaller[Future[T]] =
        new Marshaller[Future[T]] {
          override def apply(value: Future[T], ctx: MarshallingContext): Unit =
            value runAsync { tm(_, ctx) }
        }
    }

}