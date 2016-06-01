package examples {

  import net.ruffy.marble.histogram.{ Hist2D }

  object Hist2DTest {

    def main( args: Array[String] ) {

      val h = new Hist2D( 10, 0, 1, 10, 0, 1 )

      for( i <- 1 to 100000 ) {
        h.fill( java.lang.Math.random, java.lang.Math.random )
      }

      println( h.toString )

    }

  }

}
