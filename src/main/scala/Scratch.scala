import net.ruffy.marble.{ Env }
import net.ruffy.marble.math.{ VectorEnv, DoubleEnv, MathEnv, NumberEnv  }

object Scratch extends Env with DoubleEnv {

  def main( args: Array[String] ) {

    val l = Light.Rectangle(
      Vector3(  1.0f, 1.99f,  1.0f ),
      Vector3( -1.0f, 1.99f,  1.0f ),
      Vector3( -1.0f, 1.99f, -1.0f ),
      Vector3(  1.0f, 1.99f, -1.0f )
    )

    val rect = l.shape.asInstanceOf[Rectangle]
    println( l.boundingSphere.radius )
    println( (l.center -> rect.p0).abs )

  }

}
