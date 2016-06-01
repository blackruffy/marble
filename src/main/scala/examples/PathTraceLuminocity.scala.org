/*
 * 各画素から射出したフォトンが光源に当たる回数を計算する。
 */

package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, Material, Branch, Leaf, PathTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object PathTraceLuminocity {

    def main( args: Array[String] ) {

      val renderer = new PathTracer {
        val width = 500
        val height = 500
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val root = cornelBox
        val numberOfTimesToTrace = 2
        val nphoton = 50000
        val cntBuf1 = IntBuffer( width, height )
        //val cntBuf2 = IntBuffer( width, height )

        def render( x0: Int, y0: Int, x1: Int, y1: Int ): Unit = for( i <- 1 to nphoton ) {
          for( y <- y0 until y1; x <- x0 until x1 ) {
            shootPhotonRecursively( camera.createPhoton( x, y ) ) match {
              case (t, i, 2) :: _ => if( i.shape.material.surface.isLight ) cntBuf1.update( _ + 1 )( x, y )
              case _ => ()
            }
          }
          println("---- %d".format( i ))
          if( i%1000 == 0) cntBuf1.write( "images/pathtrace%dx%d_%d_nphoton_cnt%07d.mrb".format( width, height, numberOfTimesToTrace, i ) )
        }

        override def render: Unit = render( 0, 0, width, height );
      }

      renderer.render( 0, 250, 150, 251 )
      //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
      //renderer.screen.write( "png", "images/raytrace.png" )

    }

  }

}
