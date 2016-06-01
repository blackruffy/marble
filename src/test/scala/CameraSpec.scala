
import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import net.ruffy.marble._
import util._
import math._
import graphics2d._
import graphics3d._
import WrappedFloat.toWrappedFloat

class CameraSpec extends FlatSpec with MustMatchers {

  "toCamera" should "transforms a point on world coordinate to a point on camera coordinate" in {

    val p = Vector3.fromPolar( 10f, 180f.toRadians, 355f.toRadians )
    (Camera(
      new Screen( ColorBuffer( 300, 300 ) ),
      p,
      p -> Vector3.O,
      0f,
      5f
    )).toCamera( Vector3(5f, 0f, 5f) ) must be (Vector3(-5f, -5f, 10f))

  }


}
