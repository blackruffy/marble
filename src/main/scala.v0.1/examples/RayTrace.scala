package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ ColorBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, LambertCamera, Material, Branch, Leaf, RayTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object RayTrace {

    def main( args: Array[String] ) {

      val renderer = new RayTracer {
        val width = 300
        val height = 300
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        //val camera = LambertCamera( screen, 4f, 88f.toRadians, 90f.toRadians )
        val root = cornelBox
        val numberOfTimesToTrace = 5

        //render( 197, 177, 198, 178 )
        render
        //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
        //screen.image.update( 197, 177, 0xffff0000 )

        screen.write( "png", "images/raytrace.png" )

      }

    }

  }

}
