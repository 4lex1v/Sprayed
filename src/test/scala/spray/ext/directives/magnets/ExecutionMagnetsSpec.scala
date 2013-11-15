package spray.ext
package directives
package magnets

import org.scalatest.{WordSpec, Matchers}
import spray.httpx.marshalling.Marshaller
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

import ExecutionMagnets._

import scalaz.concurrent.Task

class ExecutionMagnetsSpec extends ExecutionMagnetsSpecBase {

  "Task execution magnets" should {
    "process task action" in {
      var i = 0
      val task = Task.delay { i += 1; "Test" }
      i shouldBe 0
      run(withResult(task)) {
        responseAs[String] shouldBe "Test"
        i shouldBe 1
      }
    }
  }

  "Future execution magnets" should {

  }

}

abstract class ExecutionMagnetsSpecBase
  extends WordSpec with Matchers
     with ScalatestRouteTest {

  def run[A: Marshaller](dir1: D1[A])(spec: => Unit) =
    Get("/") ~> dir1 { complete(_) } ~> check(spec)


}
