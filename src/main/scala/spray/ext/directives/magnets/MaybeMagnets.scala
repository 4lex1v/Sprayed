package spray.ext
package directives
package magnets

import spray.httpx.unmarshalling.ContentExpected
import spray.routing.Directives._

import scalaz.{Name, Maybe}

import rejections._
import MaybeMagnet._

/**
 * Copypasted from [[OptionMagnets]], but with new
 * scalaz.Maybe type (from scalaz 7.1 RC1)
 */
trait MaybeMagnets {
  def fromMaybe[A](magnet: FromMaybeMagnet[A]): D1[A] = magnet()
}

object MaybeMagnets extends MaybeMagnets

private[magnets] object MaybeMagnet {
  trait FromMaybeMagnet[A] {
    def apply(): D1[A]
  }
  object FromMaybeMagnet {
    private type FMM[A] = FromMaybeMagnet[A]
    @inline private def FMM[A](dir1: D1[A]): FMM[A] =
      new FMM[A] { override def apply(): D1[A] = dir1 }

    implicit def apply[A](maybe: Maybe[A]): FMM[A] = FMM {
      maybe.cata(provide, reject(DeserializationRejection(ContentExpected)))
    }

    /**
     * Safe'n'lazy version. Because Scala disallow syntax like (Option[A], => A), but
     * in general case we want to have the default arg by-name, we are using scalaz.Name.
     * Another possible case Lazy from shapeless 2.0
     */
    implicit def apply[A](value: (Maybe[A], Name[A])): FMM[A] = value match {
      case (opt, byName) => FMM(opt.cata(provide, provide(byName.value)))
    }
  }
}
