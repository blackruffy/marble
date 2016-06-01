
import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import net.ruffy.marble._
import util._
import math._
import WrappedFloat.toWrappedFloat

class VectorSpec extends FlatSpec with MustMatchers {

  "+" should "work" in {

    Vector3( 1, 2, 3 ) + Vector3( 4, 5, 6 ) must be (Vector3( 5, 7, 9 ))

  }

  "*" should "work" in {
    Vector3( 1, 2, 3 ) * Vector3( 4, 5, 6 ) must be (32)
    Vector3( 1, 2, 3 ) * 5f must be (Vector3(5, 10, 15))
    5f * Vector3( 1, 2, 3 ) must be (Vector3(5, 10, 15))
  }

  "x" should "work" in {
    Vector3( 1, 0, 0 ) x Vector3( 0, 1, 0 ) must be (Vector3(0, 0, 1))
  }

  "interpolate" should "work" in {
    Vector3( 5, 6, 7 ).interpolate( Vector3( 15, 4, 7 ) )( 1, 1 ) must be (Vector3(10, 5, 7))
  }

  "reflect" should "work" in {
    (Vector3( 1, -1, 0 ) reflect Vector3.Ny) must be (Vector3( 1, 1, 0 ))
  }

  "refract" should "work" in {
    val v0 = Vector3( 1,  -1, 0 ).normalize
    val n = Vector3.Ny
    val v1 = v0.refract( n, 1, 1.5f ).normalize
    //v1 must be (Vector3(2f/3, -1, 0))
    WrappedFloat(Math.sin(v0 angle (-n))/Math.sin(v1 angle n)) must be (1.5f)
  }

  "toPolar" should "work" in {
    Vector3(  0f, -1f,  0f ).toPolar must be (( 1f, Math.Pi, 0 ))
    Vector3(  0f,  1f,  0f ).toPolar must be (( 1f, 0f, 0 ))
    Vector3(  0f,  0f, -1f ).toPolar must be (( 1f, Math.Pi_2, Math.Pi + Math.Pi_2 ))
    Vector3(  0f,  0f,  1f ).toPolar must be (( 1f, Math.Pi_2, Math.Pi_2 ))
    Vector3(  1f,  0f,  0f ).toPolar must be (( 1f, Math.Pi_2, 0 ))
    Vector3( -1f,  0f,  0f ).toPolar must be (( 1f, Math.Pi_2, Math.Pi ))
  }

  "toPolar by phi" should "work" in {
    for( phi <- 0 until 360 by 5 ) {
      val v = Vector3.fromPolar( 1, 10f.toRadians, phi.toFloat.toRadians )
      val p = v.toPolar._3
      //println( phi + " == " + p.toDegrees + ": " + v)
      WrappedFloat(p) must be (phi.toFloat.toRadians)
    }
  }

  "toPolar by theta" should "work" in {
    for( theta <- 0 until 180 by 5 ) {
      val v = Vector3.fromPolar( 1, theta.toFloat.toRadians, 10f.toRadians )
      val t = v.toPolar._2
      //println( theta + " == " + t.toDegrees + ": " + v)
      WrappedFloat(t) must be (theta.toFloat.toRadians)
    }
  }

  "fromPolar around the border of theta" should "work well" in {
    val v1 = Vector3.fromPolar( 1, 170f.toRadians, 30f.toRadians )
    val v2 = Vector3.fromPolar( 1, 190f.toRadians, 30f.toRadians )

    println( (v1, v2) )
    println( (Vector3.toStringAsDegrees(v1.toPolar), Vector3.toStringAsDegrees(v2.toPolar)) )

  }

}
