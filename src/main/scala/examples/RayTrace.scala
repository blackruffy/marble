package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{
    ImageEnv, ColorEnv, GraphicsEnv
  }
  import net.ruffy.marble.graphics3d.renderer.{
    ScreenEnv,
    DefaultCameraEnv,
    LambertCameraEnv,
    MaterialEnv,
    RenderableTreeEnv,
    RendererEnv
  }
  import net.ruffy.marble.graphics3d.modeler.{
    RectangleEnv,
    BoundingSphereEnv,
    LightEnv
  }
  import net.ruffy.marble.math.{
    MathEnv,
    VectorEnv,
    MatrixEnv,
    DoubleEnv
  }

  import net.ruffy.marble.{ Env }

  object RayTrace extends Env with DoubleEnv {

    def main( args: Array[String] ) {

      val renderer = new RayTracer {
        val width = 500
        val height = 500
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera(
          screen, 17, 88.toRadians, 90.toRadians, 5
        )
        //val camera = LambertCamera(
        //  screen, 4f, 88f.toRadians, 90f.toRadians
        //)
        val root = cornelBox
        val numberOfTimesToTrace = 5

        //render(       0,        0, width/2, height/2 )
        //render( 197, 177, 198, 178 )
        //render( width/4, height/2, width/4+1, height/2+1 )
        //render( 50, 50, 51, 51 )
        //render( 0, 45, 100, 55 )

        //render
        //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
        //screen.image.update( 197, 177, 0xffff0000 )
        renderParallel( 2, 2 )

        println("writing image")
        screen.image.write("png", "images/raytrace.png")

      }

    }

  }

}
