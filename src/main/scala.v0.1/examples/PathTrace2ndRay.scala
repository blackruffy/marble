/*
 * 直接光と２回目に衝突した拡散面からの間接光のみを考慮したパストレーシング
 */

package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, ColorsBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, Material, Branch, Leaf, PathTracer, Intersection }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object PathTrace2ndRay {

    def main( args: Array[String] ) {

      val renderer = new PathTracer {
        val width = 500
        val height = 500
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val root = cornelBox
        val numberOfTimesToTrace = 5
        val nphoton = 50000
        val buf1 = ColorBuffer( width, height ) // 拡散面への直接光
        val buf2 = ColorBuffer( width, height ) // 拡散面への間接光

        def render( x0: Int, y0: Int, x1: Int, y1: Int ): Unit = for( ip <- 1 to nphoton ) {
          println("---- %d".format( ip ))
          for( y <- y0 until y1; x <- x0 until x1 ) {
            shootPhoton( camera.createPhoton( x, y ) ) match {
              case None => buf1.update( _ + Color.Black )(x, y)
              case Some((Material.Emittance, i)) => buf1.update( _ + i.shape.material.surface.emissionColor)(x, y)
              // １次レイが拡散面に衝突。２次レイを放つ。
              case Some((_, i)) => shootPhoton( i.createPhotonOnDiffuse ) match {
                case None => buf1.update( _ + Color.Black )(x, y)
                // ２次レイが光源に衝突。拡散面への直接光を計算
                case Some((Material.Emittance, j)) => buf1.update( _ + (diffuseColor(i) * j.shape.material.surface.emissionColor).normalize )(x, y)
                // ２次レイが拡散面に衝突。３次レイを放つ。
                case Some((_, j)) => shootPhoton( j.createPhotonOnDiffuse ) match {
                  case None => buf2.update( _ + Color.Black )(x, y)
                  // ３次レイが光源に衝突。拡散面への間接光を計算
                  case Some((Material.Emittance, k)) => buf2.update( _ + (diffuseColor(i) * diffuseColor(j) * k.shape.material.surface.emissionColor).normalize )(x, y)
                  // ３次レイが拡散面に衝突。トレース終了
                  case _ => buf2.update( _ + Color.Black )(x, y)
                }
              }
            }
          }
          buf1.write( "images/pathtrace_1ray%dx%d_%dpaths_cols.mrb".format(width, height, nphoton) )
          buf2.write( "images/pathtrace_2ray%dx%d_%dpaths_cols.mrb".format(width, height, nphoton) )
          buf1.write( "png", "images/pathtrace_1ray%dx%d_%dpaths_cols.png".format(width, height, nphoton) )
          buf2.write( "png", "images/pathtrace_2ray%dx%d_%dpaths_cols.png".format(width, height, nphoton) )
        }

        override def render(): Unit = render( 0, 0, width, height )

      }

      //renderer.render( 0, 240, 150, 260 )
      renderer.render()
      //root.drawWireFrame( Graphics( screen.image ), camera, 0xffffffff )
      //renderer.screen.write( "png", "images/raytrace.png" )

    }

  }

}
