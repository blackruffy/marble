package net.ruffy.marble.graphics2d.shapes {

  import net.ruffy.marble.math.{ VectorEnv }

  trait RectangleEnv {

    env: VectorEnv with Shape2DEnv =>

    case class Rectangle( val p0: Vector2, val p1: Vector2, val p2: Vector2, val p3: Vector2 ) extends Shape2D {
      val points = List(p0, p1, p2, p3)
    }

  }

}
