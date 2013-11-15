package spray.ext
package marshallers

import spray.httpx.marshalling.{MarshallerM, MarshallingContext, Marshaller}

import scalaz.{EitherT, \/}

trait DisjunctionMarshallers {
  implicit def disjunctionMarshaller[α, β](implicit mα: Marshaller[α], mβ: Marshaller[β]): Marshaller[α \/ β] = {
    new Marshaller[α \/ β] {
      override def apply(value: α \/ β, ctx: MarshallingContext): Unit = {
        value.fold(mα(_, ctx), mβ(_, ctx))
      }
    }
  }

  implicit def eitherTMarshaller[M[_]: MarshallerM, α: Marshaller, β: Marshaller] =
    new Marshaller[EitherT[M, α, β]] {
      override def apply(value: EitherT[M, α, β], ctx: MarshallingContext): Unit = {
        implicitly[MarshallerM[M]].marshaller[α \/ β].apply(value.run, ctx)
      }
    }

}

object DisjunctionMarshallers extends DisjunctionMarshallers
