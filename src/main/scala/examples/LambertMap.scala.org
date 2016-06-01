package examples {

  import net.ruffy.marble.graphics2d.{ Color }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, LambertCamera, RayTracer }
  import net.ruffy.marble.math.{ Math, Vector3 }
  import Math._

  object LambertMap {

    def main( args: Array[String] ) {

      val renderer = new RayTracer {
        val width = 300
        val height = 300
        val screen = Screen( width, height )
        val root = cornelBox
        val camera = {
          val cam = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
          root.toCamera( cam ).nearestIntersection( cam.createPhoton(width/8, height/2) ) match {
            case Some( i ) =>
              val wp = cam.toWorld( i.point )
              val wq = cam.toWorld( i.point + i.normal )
              LambertCamera( screen, wp, wp -> wq, 0f )
            case _ => LambertCamera( screen, 5f, 88f.toRadians, 90f.toRadians )
          }
        }
        //val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val numberOfTimesToTrace = 5
      }

      //renderer.render( 0, 240, 150, 260 )
      renderer.render
      //renderer.screen.image.update(renderer.screen.width/8, renderer.screen.height/2, Color.White)
      //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
      renderer.screen.write( "png", "images/raytrace_lambert.png" )

    }

  }

}
