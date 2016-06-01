
import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import net.ruffy.marble._
import util._
import math._
import WrappedFloat.toWrappedFloat

class MatrixSpec extends FlatSpec with MustMatchers {

  "The coordinates system" should "be transformed by the new direction of x axis and y axis defined on the original system." in {

    val o = Vector3( 1f/Math.sqrt(2), 1f, 1f/Math.sqrt(2) ).normalize // new origin
    //val o = Vector3.Nx // new origin
    val nz = o -> Vector3.O // new z axis
    val nx = Vector3.Ny x nz // new x axis
    val ny = nz x nx

    val t = Matrix4x4.transform(o, nx, nz).inverse

    println( t * o );
    println( t * (o -> nx) )

    (t * o) must be (Vector3.O)
    (t * (o + nx.normalize)) must be (Vector3.Nx)
    (t * (o + ny.normalize)) must be (Vector3.Ny)
    (t * (o + nz.normalize)) must be (Vector3.Nz)
    (t * Vector3.O) must be (Vector3(0f, 0f, nz.abs))

  }

}
