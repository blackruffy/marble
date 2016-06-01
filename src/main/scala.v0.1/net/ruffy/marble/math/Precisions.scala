package net.ruffy.marble.math {

  trait Maths {

    import scala.{ Double => SDouble, Float => SFloat }

    type NumType
    type Num <: Number[NumType, Num]

    protected implicit def create( x: SDouble ): Num
    protected implicit def create( x: SFloat ): Num

    lazy val precision: Num = 0.0001
    //val precision: Num = 100f
    val Pi: Num = math.Pi
    val Pi_2: Float = Pi/2.0
    val Pix2: Float = Pi*2.0

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
      val vy = n.normalize
      val vz = vy x Vector3.Ny
      val vx = vz x vy

      val theta = n angle x

      val uz = x x n
      val x2 = Matrix4x4.rotation( -uz, Math.Pi_2 - theta ) * x
      val phi = x2 angle vx

      (theta, phi)
    }

  }

}
