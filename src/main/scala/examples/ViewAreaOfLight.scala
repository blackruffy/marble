package examples {

  import net.ruffy.marble.histogram.{ Hist2D, Hist1D }
  import net.ruffy.marble.math.{ DoubleEnv }
  import net.ruffy.marble.graphics3d.renderer.{ PhotonEnv }
  import net.ruffy.marble.{ Env }


  object ViewAreaOfLight extends Env with DoubleEnv {

    def main( args: Array[String] ) {

      val rect = Light.Rectangle(
        Vector3(  1.0, 2.0,  1.0 ),
        Vector3( -1.0, 2.0,  1.0 ),
        Vector3( -1.0, 2.0, -1.0 ),
        Vector3(  1.0, 2.0, -1.0 )
      )

      {
        val point = Vector3( 0.0, -2.0, 0.0 )
        val normal = Vector3.Ny
        println( rect.viewAreaStat( point, normal, 1000 ) )
      }

      {
        val point = Vector3( -2.0, -2.0, 0.0 )
        val normal = Vector3.Nx
        println( rect.viewAreaStat( point, normal, 1000 ) )
      }

      {
        val point = Vector3( 0.0, 0.0, 2.0 )
        val normal = -Vector3.Nz
        println( rect.viewAreaStat( point, normal, 1000 ) )
      }

    }

  }

}
