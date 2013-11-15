package spray.ext

import spray.httpx.unmarshalling.DeserializationError
import spray.routing.Rejection

object rejections {
  case class DeserializationRejection(de: DeserializationError) extends Rejection
}
