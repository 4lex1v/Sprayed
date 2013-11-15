package spray.ext
package directives
package magnets

import org.scalatest.{WordSpecLike, Matchers}

import spray.ext.rejections.DeserializationRejection
import spray.httpx.marshalling.{Marshaller, ToResponseMarshallable}
import spray.httpx.unmarshalling.ContentExpected
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

import scalaz.Name
import scalaz.Maybe.{empty => nothing, _}

class MaybeMagnetsSpec extends OptionMagnetsSpecBase {
  import MaybeMagnets.fromMaybe

  "Maybe magnets" should {
    "extract value from option" in {
      run(fromMaybe(just("Hello"))) {
        responseAs[String] shouldBe "Hello"
      }
    }

    "reject request if Option == None" in {
      run(fromMaybe(nothing[String])) {
        rejection shouldBe DeserializationRejection(ContentExpected)
      }
    }

    "provide the default value" in {
      run(fromMaybe(nothing[String], Name("Default"))) {
        responseAs[String] shouldBe "Default"
      }
    }

    "not eval default arg" in {
      var i = 0
      val default = Name { i += 1; "Default" }
      run(fromMaybe(just("Test"), default)) {
        responseAs[String] shouldBe "Test"
        i shouldBe 0
      }
    }

    "eval default argument" in {
      var i = 0
      val default = Name { i += 1; "Default" }
      run(fromMaybe(nothing[String], default)) {
        responseAs[String] shouldBe "Default"
        i shouldBe 1
      }
    }

  }

}

abstract class MaybeMagnetsSpecBase
  extends WordSpecLike with Matchers
  with ScalatestRouteTest {

  def run[A: Marshaller](dir1: D1[A])(spec: => Unit) =
    Get("/") ~> dir1 { complete(_) } ~> check(spec)

}