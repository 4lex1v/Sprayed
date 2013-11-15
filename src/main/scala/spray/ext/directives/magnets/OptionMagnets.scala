package spray.ext
package directives
package magnets

import spray.httpx.unmarshalling.ContentExpected
import spray.routing.Directives._

import rejections._
import OptionMagnet._

import scalaz.{Functor, OptionT, Name}

trait OptionMagnets {
  def fromOption[A](magnet: FromOptionMagnet[A]): D1[A] = magnet()
//  def fromOptionT[M[_], A](magnet: FromOptionTMagnet[M, A]): D1[A] = magnet()
}

object OptionMagnets extends OptionMagnets

private[magnets] object OptionMagnet {
  private val rejected = reject(DeserializationRejection(ContentExpected))
  trait FromOptionMagnet[A] {
    def apply(): D1[A]
  }

  object FromOptionMagnet {
    private type FOM[A] = FromOptionMagnet[A]
    @inline private def FOM[A](dir1: D1[A]): FOM[A] =
      new FOM[A] { override def apply(): D1[A] = dir1 }

    implicit def apply[A](opt: Option[A]): FOM[A] = FOM {
      opt match {
        case Some(value) => provide(value)
        // maybe we should change it with some other rejection??
        case None => rejected
      }
    }

    /**
     * Safe'n'lazy version. Because Scala disallow syntax like (Option[A], => A), but
     * in general case we want to have the default arg by-name, we are using scalaz.Name.
     * Another possible case Lazy from shapeless 2.0
     */
    implicit def apply[A](value: (Option[A], Name[A])): FOM[A] = value match {
      case (opt, byName) => FOM(opt.fold(provide(byName.value))(provide))
    }
  }

//  trait FromOptionTMagnet[M[_], A] {
//    def apply(): D1[A]
//  }

//  object FromOptionTMagnet {
//    implicit def apply[M[_]: Functor, A](opt: OptionT[M, A]): FromOptionTMagnet[M, A] = {
//      new FromOptionTMagnet[M, A] {
//        override def apply(): D1[A] = opt.fold(provide, rejected)
//      }
//    }
//  }


}