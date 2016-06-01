
package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, ColorsBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, Material, Branch, Leaf, PathTracer, Intersection, Grid }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object GridWriter {

    def main( args: Array[String] ) {

      new PathTracer {
        val width = 500
        val height = 500
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val root = cornelBox
        val nphotons = 10000
        val map = Grid( croot.getBoundingSphere, 10 )
        val numberOfTimesToTrace = 5

        for( i <- 1 to nphotons ) {
          if( i%1000 == 0 ) println( ">> %d".format( i ) )
          lights.foreach { l =>
            shootPhoton( l.createPhoton( Color.White ) ) match {
              case Some((Material.Diffusivity, isec)) => map.update( (isec.point, isec.shape.material.surface.diffuseColor) )
              case _ => ()
            }
          }
        }

        println("writing file")
        map.write( "images/photonmap.mrb" )

        {
          println("reading file")
          val map = Grid( "images/photonmap.mrb" )
          map.write( screen.pixel _ o camera.toScreen, screen.image, "png", "images/photonmap.png" )
        }
      }

    }

  }

}
