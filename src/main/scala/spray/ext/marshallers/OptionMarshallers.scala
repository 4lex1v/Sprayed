package spray.ext
package marshallers

import spray.httpx.marshalling.{MarshallingContext, Marshaller, MarshallerM}

import scalaz.OptionT

trait OptionMarshallers {
  implicit def optionTMarshaller[M[_]: MarshallerM, A: Marshaller] =
    new Marshaller[OptionT[M, A]] {
      override def apply(value: OptionT[M, A], ctx: MarshallingContext): Unit = {
        implicitly[MarshallerM[M]].marshaller[Option[A]].apply(value.run, ctx)
      }
    }
}

object OptionMarshallers extends OptionMarshallers