package net.ruffy.marble.math {

  import scala.{ Double => SDouble, Float => SFloat }

  abstract class Number[A, B <: Number[A, B]] {

    def underlying: A

    def unary_- : B

    def + ( x: B ): B
    def - ( x: B ): B
    def * ( x: B ): B
    def / ( x: B ): B

    def > ( x: B ): Boolean
    def < ( x: B ): Boolean
    def >= ( x: B ): Boolean
    def <= ( x: B ): Boolean
    def == ( x: B ): Boolean = equals( x )

    def max( x: B ): B
    def min( x: B ): B

    def toDouble: Double
    def toFloat: Float
    def toInt: Int
    def toUnwrappedDouble: SDouble
    def toUnwrappedFloat: SFloat
    def toDegrees: B
    def toRadians: B

    def equals( x: B ): Boolean
    override val toString = underlying.toString

  }

  class Double( v: SDouble ) extends Number[SDouble, Double] {
    import Number._
    import java.lang.Math.{ abs, ulp }

    lazy val underlying = v

    lazy val unary_- : Double = v * -1

    def + ( x: Double ): Double = v + x.underlying
    def - ( x: Double ): Double = v - x.underlying
    def * ( x: Double ): Double = v * x.underlying
    def / ( x: Double ): Double = v / x.underlying

    def > ( x: Double ): Boolean = v > x.underlying
    def < ( x: Double ): Boolean = v < x.underlying
    def >= ( x: Double ): Boolean = v >= x.underlying
    def <= ( x: Double ): Boolean = v <= x.underlying

    def max( x: Double ): Double = v max x.underlying
    def min( x: Double ): Double = v min x.underlying

    def toDouble: Double = this
    def toFloat: Float = v.toFloat
    def toInt: Int = v.toInt
    def toUnwrappedDouble: SDouble = v
    def toUnwrappedFloat: SFloat = v.toFloat
    def toDegrees: Double = v.toDegrees
    def toRadians: Double = v.toRadians

    def equals( x: Double ): Boolean = abs( v - x.underlying ) < ulp( v ) * 100
  }

  class Float( v: SFloat ) extends Number[SFloat, Float] {
    import Number._
    import java.lang.Math.{ abs, ulp }

    lazy val underlying = v

    lazy val precision: SFloat = java.lang.Math.ulp(1.0f)

    lazy val unary_- : Float = v * -1

    def + ( x: Float ): Float = v + x.underlying
    def - ( x: Float ): Float = v - x.underlying
    def * ( x: Float ): Float = v * x.underlying
    def / ( x: Float ): Float = v / x.underlying

    def > ( x: Float ): Boolean = v > x.underlying
    def < ( x: Float ): Boolean = v < x.underlying
    def >= ( x: Float ): Boolean = v >= x.underlying
    def <= ( x: Float ): Boolean = v <= x.underlying

    def max( x: Float ): Float = v max x.underlying
    def min( x: Float ): Float = v min x.underlying

    def toDouble: Double = v.toDouble
    def toFloat: Float = this
    def toInt: Int = v.toInt
    def toUnwrappedDouble: SDouble = v.toDouble
    def toUnwrappedFloat: SFloat = v
    def toDegrees: Float = v.toDegrees
    def toRadians: Float = v.toRadians

    def equals( x: Float ): Boolean = abs( v - x.underlying ) < ulp( v ) * 100
  }

  object Number {

    implicit def toDouble( x: SDouble ): Double = new Double( x )
    implicit def toFloat( x: SFloat ): Float = new Float( x )
  }

}
