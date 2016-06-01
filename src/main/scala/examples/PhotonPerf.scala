package examples {

  import net.ruffy.marble.math.{ DoubleEnv }
  import net.ruffy.marble.{ Env }

  object PhotonPerf extends Env with DoubleEnv {

    def main( args: Array[String] ) {

      val va:Num = 30.toRadians
      val n = Vector3(1, 3, 2)

      {
        val (t, p) = Math.randomAngle( va )
        val a = Photon.createPhoton( Vector3.O, n, t, p, Color.White, Material.Medium.Default, None )
        val b = Photon( Vector3.O, n, t, p, Color.White, Material.Medium.Default, None )
        println( List(a, b).mkString("\n") )
      }

      val t1 = System.nanoTime
      for( i <- 1 to 10000 ) {
        val (t, p) = Math.randomAngle( va )
        Photon.createPhoton( Vector3.O, n, t, p, Color.White, Material.Medium.Default, None )
      }
      println( System.nanoTime - t1 )

      val t2 = System.nanoTime
      for( i <- 1 to 10000 ) {
        val (t, p) = Math.randomAngle( va )
        Photon( Vector3.O, n, t, p, Color.White, Material.Medium.Default, None )
      }
      println( System.nanoTime - t2 )

      val t3 = System.nanoTime
      for( i <- 1 to 10000 ) {
        val (t, p) = Math.randomAngle( va )
        Photon.createPhoton( Vector3.O, n, t, p, Color.White, Material.Medium.Default, None )
      }
      println( System.nanoTime - t3 )

    }

  }

}
