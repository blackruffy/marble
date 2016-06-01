package net.ruffy.marble.graphics2d.shapes {

  import net.ruffy.marble.math.{ VectorEnv }

  trait Shape2DEnv {

    env: VectorEnv =>

    abstract class Shape2D {

      val points: List[Vector2]

    }

  }

}
