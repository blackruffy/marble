/*
 * 
 */

package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, ColorsBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, Material, Branch, Leaf, PathTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object PathTrace {

    def main( args: Array[String] ) {

      val renderer = new PathTracer {
        val width = 500
        val height = 500
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val root = cornelBox
        val numberOfTimesToTrace = 50
        val nphoton = 20
        val typeSize = ColorsBuffer.typeSize( numberOfTimesToTrace )
        val buf = ColorsBuffer( width, height, typeSize )

        def render( x0: Int, y0: Int, x1: Int, y1: Int ): Unit = for( ip <- 1 to nphoton ) {
          for( y <- y0 until y1; x <- x0 until x1 ) {
            shootPhotonRecursively( camera.createPhoton( x, y ) ) match {
              case Nil => buf.update(x, y, Nil)
              case (t, i, n) :: _ => 
                val cols = if( i.shape.material.surface.isLight ) i.shape.material.surface.color( t ) :: extractColors( i.ray ) else Nil
                buf.update(x, y, cols)
            }
          }
          println("---- %d".format( ip ))
          buf.write( "images/pathtrace%dx%d_%d_cols_cnt%d.mrb".format(width, height, numberOfTimesToTrace, ip) )
        }

        override def render(): Unit = render( 0, 0, width, height )

      }

      renderer.render( 0, 240, 150, 260 )
      //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
      //renderer.screen.write( "png", "images/raytrace.png" )

    }

  }

}
