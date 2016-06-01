package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ ColorBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, LambertCamera, Material, Branch, Leaf, RayTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4, Math }

  object Irradiance {

    def main( args: Array[String] ) {

      val renderer = new RayTracer {
        val width = 300
        val height = 300
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        //val camera = LambertCamera( screen, 4f, 88f.toRadians, 90f.toRadians )
        val root = cornelBox
        val numberOfTimesToTrace = 5

        // スクリーン
        val rect = Rectangle(
          Vector3(1f, 1f, 5f),
          Vector3(1f, -1f, 5f),
          Vector3(-1f, -1f, 5f),
          Vector3(-1f, 1f, 5f)
        )

        croot.nearestIntersection( camera.createPhoton( 70, 150 ) ) foreach { i =>
          val ray = i.point -> Vector3.O
          println( rect.intersect( i.createPhoton( ray ) ) )
          val (th, ph) = Math.angles( i.normal, ray )
          println((th, ph))
          val p = i.createPhoton( th, ph )
          println( Math.angles( i.normal, p.direction ) )
          println( (p.direction angle (i.point -> Vector3.O)).toDegrees )
          println( rect.intersect( p ) )
          //val dt = 0.01f
          //println( th.toDegrees, ph.toDegrees )
          //for( n <- 1 to 1 ) {
          //  if( n%1000 == 0 ) println( n )
          //  println(((th - dt).toDegrees, (th + dt).toDegrees, (ph - dt).toDegrees, (ph + dt).toDegrees))
          //  val photon = i.createPhotonOnDiffuse(th - dt, th + dt, ph - dt, ph + dt)
          //  println( Math.toDegrees( Math.angles(i.normal, photon.direction ) ) )
          //  println( (photon.direction angle (i.point -> Vector3.O)).toDegrees )
          //  rect.intersect( photon ) foreach { j =>
          //    println("****")
          //    val (x, y, z) = camera.screen.pixel( j.point )
          //    if( x == 70 && y == 150 ) println("======")
          //  }
          //}
        }

        //render( 197, 177, 198, 178 )
        //render
        //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
        //screen.image.update( 197, 177, 0xffff0000 )

        //screen.write( "png", "images/raytrace.png" )

      }

    }

  }

}
