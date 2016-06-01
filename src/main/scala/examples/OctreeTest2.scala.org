package examples {

  import java.nio.{ ByteBuffer }
  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, ColorsBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, Material, Branch, Leaf, PathTracer, Intersection, Octree, PhotonMap => PM, PhotonMapper }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }

  object OctreeTest2 {

    def main( args: Array[String] ) {

      val renderer = new PhotonMapper {
        val width = 300
        val height = 300
        val screen = Screen( width, height )
        val root = cornelBox
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val numberOfTimesToTrace = 5
        val numberOfPhotons = 1000
        val radius = 0.3f
        //val map = PM.octree( "images/octree_test.mrb" ).underlying
        val map = createPhotonMap.underlying

        croot.nearestIntersection( camera.createPhoton( 55, 104 ) ) foreach { isec =>
          println( isec.point )

          val b = BoundingSphere( isec.point, 0.5f )

          //map.find( b.center ).foreach { o => println( o, o.size ) }

          map.filterChildren( b, { (_, oct) =>
            println("----- %s, b:%s".format( oct, oct.boundingSphere.contains( b ) ))
            //oct.children_.foreach { _.foreach { o1 =>
            //  println( o1.boundingSphere.contains( b ), o1.size )
            //} }
            //oct.foreach { (p, c) =>
            //  //println( oct.boundingSphere.inside_?( p ) )
            //  if( (p -> b.center).abs < b.radius )
            //    println("********** %s".format(c))
            //}
          } )
        }

      }

    }

  }

}
