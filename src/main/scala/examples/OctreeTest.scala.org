package examples {

  import java.nio.{ ByteBuffer }
  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, ColorsBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, Material, Branch, Leaf, PathTracer, Intersection, Octree }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object OctreeTest {

    def main( args: Array[String] ) {

      new PathTracer {
        val width = 500
        val height = 500
        val screen = new Screen( ColorBuffer( width, height ) )
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val root = cornelBox
        val nphotons = 1000
        val map = Octree[Color]( croot.getBoundingSphere )
        val numberOfTimesToTrace = 5

        for( i <- 1 to nphotons ) {
          if( i%1000 == 0 ) println( ">> %d".format( i ) )
          lights.foreach { l =>
            shootPhoton( l.createPhoton( Color.White ) ) match {
              case Some((Material.Diffusivity, isec)) => map.add( isec.point, isec.shape.material.surface.diffuseColor )
              case _ => ()
            }
          }
        }

        println("writing file")
        map.write( "images/octree_test.mrb", 16, { (buf, c) =>
          buf.putFloat( c.alpha )
          buf.putFloat( c.red )
          buf.putFloat( c.green )
          buf.putFloat( c.blue )
        } )
        
        {
          println("reading file")
          val map = Octree[Color]( "images/octree_test.mrb", { b: ByteBuffer => Color(
            b.getFloat,
            b.getFloat,
            b.getFloat,
            b.getFloat
          ) } )
        
          map.foreach { (p, c) =>
            val (x, y, z) = screen.pixel( camera.toScreen( p ) )
            screen.image.update( x, y, c )
          }
        
          screen.image.write( "png", "images/octree_test.png" )
        
        }
      }

    }

  }

}
