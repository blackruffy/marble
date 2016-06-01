package examples {

  object ObjectUpdateCosts {

    class MObject(
      var a0: Int,
      var a1: Int,
      var a2: Int,
      var a3: Int,
      var a4: Int,
      var a5: Int,
      var a6: Int,
      var a7: Int,
      var a8: Int,
      var a9: Int,
      var a10: Int,
      var a11: Int,
      var a12: Int,
      var a13: Int,
      var a14: Int,
      var a15: Int,
      var a16: Int,
      var a17: Int,
      var a18: Int,
      var a19: Int
    ) {
      def update( a: Int ) = a10 = a
    }

    class IObject(
      val a0: Int,
      val a1: Int,
      val a2: Int,
      val a3: Int,
      val a4: Int,
      val a5: Int,
      val a6: Int,
      val a7: Int,
      val a8: Int,
      val a9: Int,
      val a10: Int,
      val a11: Int,
      val a12: Int,
      val a13: Int,
      val a14: Int,
      val a15: Int,
      val a16: Int,
      val a17: Int,
      val a18: Int,
      val a19: Int
    ) {

      def update( a: Int ) = new IObject(
        a0,
        a1,
        a2,
        a3,
        a4,
        a5,
        a6,
        a7,
        a8,
        a9,
        a,
        a11,
        a12,
        a13,
        a14,
        a15,
        a16,
        a17,
        a18,
        a19
      )

    }

    def main( args: Array[String] ) {

      val m = new MObject( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 )
      val i = new IObject( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 )

      {
        val t0 = System.nanoTime
        for( x <- 1 to 10000 ) m.update( x )
        println( System.nanoTime - t0 )
      }

      {
        val t0 = System.nanoTime
        for( x <- 1 to 10000 ) i.update( x )
        println( System.nanoTime - t0 )
      }

    }

  }

}
