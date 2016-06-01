
import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import net.ruffy.marble._
import util._
import math._
import WrappedFloat.toWrappedFloat

class MathSpec extends FlatSpec with MustMatchers {

  "equals" should "work" in {

    (1.0f ~== 0.99999999f) must be (true)

    (1.0f ~== 0.99f) must be (false)

    (1.0f ~== 1234f) must be (false)

    (1.0f ~== 0.09f) must be (false)

    (1234.0f ~== 0.99f) must be (false)

  }

}
