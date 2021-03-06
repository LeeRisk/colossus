package colossus
package testkit

import colossus.service.{UnmappedCallback, Callback}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.Try

class CallbackMatchersSpec extends WordSpec with MustMatchers{

  import CallbackMatchers._

  "A CallbackMatcher" should {

    "fail to match if a Callback never executes" in {

      def cbFunc(f : Try[Int] => Unit) {

      }

      val unmapped = UnmappedCallback(cbFunc)

      var execd = false
      val result = new CallbackEvaluateTo[Int](a => execd = true).apply(unmapped)
      result.matches must equal(false)
      execd must equal(false)

    }

    "fail to match if the 'evaluate' function throws" in {
      var execd = false
      val cb = Callback.successful("success!")
      val eval = (a : String) => {
        execd = true
        a must equal("awesome!")
      }
      val result = new CallbackEvaluateTo[String](eval).apply(cb)
      result.matches must equal(false)
      execd must equal(true)
    }

    "fail to match if a Callback reports a failure" in {
      val cb = Callback.failed(new Exception("bam"))
      var execd = false
      val result = new CallbackEvaluateTo[Any](a => execd = true).apply(cb)
      result.matches must equal(false)
      execd must equal(false)
    }

    "success if the callback successfully executes the evaluate function" in {
      var execd = false
      val cb = Callback.successful("success!")
      val eval = (a : String) => {
        execd = true
        a must equal("success!")
      }
      val result = new CallbackEvaluateTo[String](eval).apply(cb)
      result.matches must equal(true)
      execd must equal(true)
    }


  }

}
