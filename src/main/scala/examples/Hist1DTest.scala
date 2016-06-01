package examples {

  import net.ruffy.marble.histogram.{ Hist1D }

  object Hist1DTest {

    def main( args: Array[String] ) {

      val h = new Hist1D( 10, 0, 1 )

      for( i <- 1 to 100 ) {
        h.fill( java.lang.Math.random )
      }

      println( h.toString )

    }

  }

}
