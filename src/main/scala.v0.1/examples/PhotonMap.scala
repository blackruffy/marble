package examples {

  import net.ruffy.marble.graphics2d.{ Color }
  import net.ruffy.marble.graphics3d.{ Screen, DefaultCamera, PhotonMapper, PhotonMap => PM }
  import net.ruffy.marble.math.{ Math, Vector3 }
  import Math._
  import net.ruffy.util.{ Implicits }
  import Implicits._
  import java.io.File

  object PhotonMap {

    def main( args: Array[String] ) {

      new PhotonMapper {
        val width = 500
        val height = 500
        val screen = Screen( width, height )
        val root = cornelBox
        val camera = DefaultCamera( screen, 17f, 88f.toRadians, 90f.toRadians, 5 )
        val numberOfTimesToTrace = 5
        val numberOfPhotons = 1000000
        val radius = 0.2f

        val fname = "images/photonmap_%d.dat".format( numberOfPhotons )

        val map = if( (new File(fname)).exists ) {
          readPhotonMap( fname )
        } else {
          val m = createPhotonMap
          m.write( fname )
          m
        }

        //val map = createPhotonMap

        //croot.nearestIntersection( camera.createPhoton( 55, 130 ) ) foreach { isec =>
        //
        //  map foreach { (p, q) =>
        //    val x = p
        //    val d = (isec.point -> x).abs
        //    val t = (isec.point -> x) angle isec.normal
        //    if( d < radius ) {
        //      println( "%s, %s, %f, %f".format( isec.point, x, d, t.toDegrees ) )
        //    }
        //  }
        //
        //}

        //map.write(
        //  renderer.screen.pixel _ o renderer.camera.toScreen _,
        //  renderer.screen.image,
        //  "png", "images/photonmap_%d.png".format( numberOfPhotons )
        //)

        //render( map, 55, 100, 56, 101 )
        render( map )
        //render
        screen.write( "png", "images/raytrace_photonmap_%dx%d_%d_%.2f.png".format( width, height, numberOfPhotons, radius ) )
      }

    }

  }

}
