package net.ruffy.marble.graphics2d {

  class ZBuffer( width: Int, height: Int ) {

    val buf = new Array[Array[Int]]( width )

    for( x <- 0 until width ) {
      val bufy = new Array[Int]( height )
      buf( x ) = bufy
      for( y <- 0 until height ) {
        bufy( y ) = Int.MaxValue
      }
    }

    def apply( x: Int, y: Int ) = if( x >= 0 && x < width && y >= 0 && y < height ) buf( x )( y ) else Int.MaxValue

    def update( x: Int, y: Int, z: Int ) = if( x >= 0 && x < width && y >= 0 && y < height ) buf( x )( y ) = z

  }

}
