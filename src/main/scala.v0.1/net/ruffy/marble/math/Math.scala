package net.ruffy.marble.math {

  class WrappedFloat( val x: Float ) {
    override def equals( a: Any ): Boolean = a match {
      case b: WrappedFloat => Math.equals( x )( b.x )
      case b: Float => Math.equals( x )( b )
      case _ => false
    }
    def ~== ( a: Any ): Boolean = equals( a )
    def * [A <: Vector[A]]( v: A ): A = v * x
    override def toString = x.toString
  }

  object WrappedFloat {
    def apply( x: Float ): WrappedFloat = new WrappedFloat( x )
    implicit def toWrappedFloat( x: Float ): WrappedFloat = new WrappedFloat( x )
  }

  object Math {

    val precision: Float = 0.0001f
    //val precision: Float = 100f
    val Pi: Float = math.Pi.toFloat
    val Pi_2: Float = Pi/2f
    val Pix2: Float = Pi*2f

    def equals( a: Float, b: Float, precision: Float ): Boolean = java.lang.Math.abs( a - b ) < precision
    //def equals( a: Float, b: Float, precision: Float ): Boolean = {
    //  val ua = java.lang.Math.ulp(a)
    //  val ub = java.lang.Math.ulp(b)
    //  java.lang.Math.abs( a - b ) < (if(ua > ub) ua else ub)*precision
    //}

    def max( x: Float, y: Float ): (Float, Float) = if( x > y ) (x, y) else (y, x)

    def min( x: Float, y: Float ): (Float, Float) = if( x < y ) (x, y) else (y, x)

    def equals( a: Float )( b: Float ): Boolean = equals( a, b, precision )

    def pow2( x: Float ): Float = x * x

    def pow( x: Float, y: Float ): Float = scala.math.pow( x, y ).toFloat

    def random: Float = scala.math.random.toFloat

    def abs( x: Float ): Float = java.lang.Math.abs( x )

    def sqrt( x: Float ): Float = math.sqrt( x ).toFloat

    def asin( x: Float ): Float = math.asin( x ).toFloat

    def acos( x: Float ): Float = math.acos( x ).toFloat

    def atan( x: Float ): Float = math.atan( x ).toFloat

    def atan2( x: Float, y: Float ): Float = math.atan2( x, y ).toFloat

    def cos( x: Float ): Float = math.cos( x ).toFloat

    def sin( x: Float ): Float = math.sin( x ).toFloat

    def toDegree( r: Float ): Float = r.toDegrees

    def toRadian( d: Float ): Float = d.toRadians

    def toDegrees( n: (Float, Float) ): (Float, Float) = {
      val (x, y) = n
      (x.toDegrees, y.toDegrees)
    }

    /**
      * ベクトルnとベクトルxがなす角と、ベクトルn周りの回転角を求める。
      */
    def angles( n: Vector3, x: Vector3 ): (Float, Float) = {
      val mx = x.normalize

      val vy = n.normalize
      val vz = (vy x Vector3.Ny).normalize
      val vx = (vz x vy).normalize

      val theta = vy angle mx

      val uz = (mx x vy).normalize
      val x2 = (Matrix4x4.rotation( -uz, Math.Pi_2 - theta ) * mx).normalize
      val phi = x2 angle vx

      (theta, phi)
    }

  }

}
