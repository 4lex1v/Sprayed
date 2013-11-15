package spray.ext
package marshallers

import spray.httpx.marshalling.{MarshallerM, MarshallingContext, Marshaller}
import spray.httpx.marshalling.MetaMarshallers.optionMarshaller
import spray.httpx.marshalling.MarshallerM.optionMarshallerM

import scalaz.syntax.functor._
import scalaz.{Functor, MaybeT, Maybe}

trait MaybeMarshallers {

  // Replace concrete optionMarshaller call with MarshallerM[Option]
  implicit def maybeMarshaller[A: Marshaller] =
    new Marshaller[Maybe[A]] {
      override def apply(value: Maybe[A], ctx: MarshallingContext): Unit =
        optionMarshaller[A].apply(value.toOption, ctx)
    }

  implicit def maybeTMarshaller[M[_]: Functor: MarshallerM, A: Marshaller] =
    new Marshaller[MaybeT[M, A]] {
      override def apply(value: MaybeT[M, A], ctx: MarshallingContext): Unit =
        implicitly[MarshallerM[M]].marshaller[Option[A]]
          .apply(value.run.map(_.toOption), ctx)
    }

}

object MaybeMarshallers extends MaybeMarshallers
