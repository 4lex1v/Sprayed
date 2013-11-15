package spray.ext
package directives
package magnets

import spray.ext.rejections.DeserializationRejection
import spray.httpx.marshalling.{Marshaller, ToResponseMarshallable}
import spray.httpx.unmarshalling.ContentExpected
import spray.routing.Directives._
import scalaz.Name
import spray.testkit.ScalatestRouteTest
import org.scalatest.{WordSpecLike, Matchers}
import scalaz.std.option._

class OptionMagnetsSpec extends OptionMagnetsSpecBase {
  import OptionMagnets.fromOption

  "Option magnets" should {
    "extract value from option" in {
      run(fromOption(Option("Hello"))) {
        responseAs[String] shouldBe "Hello"
      }
    }

    "reject request if Option == None" in {
      run(fromOption(none[String])) {
        rejection shouldBe DeserializationRejection(ContentExpected)
      }
    }

    "provide the default value" in {
      run(fromOption(none[String], Name("Default"))) {
        responseAs[String] shouldBe "Default"
      }
    }

    "not eval default arg" in {
      var i = 0
      val default = Name { i += 1; "Default" }
      run(fromOption(some("Test"), default)) {
        responseAs[String] shouldBe "Test"
        i shouldBe 0
      }
    }

    "eval default argument" in {
      var i = 0
      val default = Name { i += 1; "Default" }
      run(fromOption(none[String], default)) {
        responseAs[String] shouldBe "Default"
        i shouldBe 1
      }
    }

  }

}

abstract class OptionMagnetsSpecBase
  extends WordSpecLike with Matchers
     with ScalatestRouteTest {

  def run[A: Marshaller](dir1: D1[A])(spec: => Unit) =
    Get("/") ~> dir1 { complete(_) } ~> check(spec)

}
