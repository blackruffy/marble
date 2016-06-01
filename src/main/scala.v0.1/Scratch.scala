

object Scratch {

  import net.ruffy.marble._
  import graphics2d._
  import graphics3d._
  import graphics3d.shapes._
  import math._

  def main( args: Array[String] ) {

    println( Math.toDegrees( Math.angles( Vector3(1f, 1f, 0f), Vector3(1f, 0.5f, 0f) ) ) )
    println( Math.toDegrees( Math.angles( Vector3(1f, 1f, 0f), Vector3(1f, 0.5f, 1f) ) ) )

  }

}
