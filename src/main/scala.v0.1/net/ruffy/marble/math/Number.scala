package net.ruffy.marble.math {

  import scala.{ Double => SDouble, Float => SFloat }

  abstract class Number[A, B <: Number[A, B]] {

    def underlying: A

    def + ( x: B ): B
    def - ( x: B ): B
    def * ( x: B ): B
    def / ( x: B ): B

    def > ( x: B ): Boolean
    def < ( x: B ): Boolean
    def == ( x: B ): Boolean

    def toDouble: Double
    def toFloat: Float
    def toDegrees: B
    def toRadians: B

  }

  class Double( v: SDouble ) extends Number[SDouble, Double] {
    import Number._

    lazy val underlying = v

    def + ( x: Double ): Double = v + x.underlying
    def - ( x: Double ): Double = v - x.underlying
    def * ( x: Double ): Double = v * x.underlying
    def / ( x: Double ): Double = v / x.underlying

    def > ( x: Double ): Boolean = v > x.underlying
    def < ( x: Double ): Boolean = v < x.underlying
    def == ( x: Double ): Boolean = v == x.underlying

    def toDouble: Double = this
    def toFloat: Float = v.toFloat
    def toDegrees: Double = v.toDegrees
    def toRadians: Double = v.toRadians

  }

  class Float( v: SFloat ) extends Number[SFloat, Float] {
    import Number._

    lazy val underlying = v

    def + ( x: Float ): Float = v + x.underlying
    def - ( x: Float ): Float = v - x.underlying
    def * ( x: Float ): Float = v * x.underlying
    def / ( x: Float ): Float = v / x.underlying

    def > ( x: Float ): Boolean = v > x.underlying
    def < ( x: Float ): Boolean = v < x.underlying
    def == ( x: Float ): Boolean = v == x.underlying

    def toDouble: Double = v.toDouble
    def toFloat: Float = this
    def toDegrees: Float = v.toDegrees
    def toRadians: Float = v.toRadians

  }

  object Number {

    implicit def toDouble( x: SDouble ): Double = new Double( x )
    implicit def toDouble( x: SFloat ): Double = new Double( x,toDouble )
    implicit def toFloat( x: SDouble ): Float = new Float( x.toFloat )
    implicit def toFloat( x: SFloat ): Float = new Float( x )

  }

}
