package examples {

  import net.ruffy.marble.histogram.{ Hist2D, Hist1D }
  import net.ruffy.marble.math.{ DoubleEnv }
  import net.ruffy.marble.{ Env }
  import net.ruffy.marble.graphics3d.renderer.{ PhotonEnv }

  object RandomPhotonTest extends Env with DoubleEnv {

    def main( args: Array[String] ) {

      val rect = Light.Rectangle(
        Vector3(  1.0, 2.0,  1.0 ),
        Vector3( -1.0, 2.0,  1.0 ),
        Vector3( -1.0, 2.0, -1.0 ),
        Vector3(  1.0, 2.0, -1.0 )
      )

      val point = Vector3( 0.0, -2.0, 0.0 )
      val normal = Vector3.Ny
//      rect.viewAngle( point, normal ) foreach { case (v, a) =>
//        println( a.toDegrees )
//        val h1 = new Hist1D( 10, 0, va.toUnwrappedDouble )
//        val h2 = new Hist1D( 10, 0, 360.toRadians.toUnwrappedDouble )
//        val h3 = new Hist2D( 10, 0, va.toUnwrappedDouble, 10, 0, 360.toRadians.toUnwrappedDouble )
//
//        var cnt1: Num = 0
//        var cnt2: Num = 0
//        for( i <- 1 to 1000 ) {
//          val (t, p) = Math.randomAngle( va )
//          val photon = Photon.createPhoton( point, normal, t + a1, p, Color.White, Material.Medium.Default, None )
//          rect.intersect( photon ) foreach { _ =>
//            cnt1 += 1
//            h1.fill( t.underlying )
//            h2.fill( p.underlying )
//            h3.fill( t.underlying, p.underlying )
//          }
//          cnt2 += 1
//        }
//
//        println( h1.toString )
//        println( "---" )
//        println( h2.toString )
//        println( "---" )
//        println( h3.toString )
//        println( "---" )
//        println( (cnt1, cnt2, cnt1/cnt2, 4.0/Math.Pix2, Math.sphereSurfaceIntegral( 1, a1, a2, 0, Math.Pix2 )) )
//      }

    }

  }

}
