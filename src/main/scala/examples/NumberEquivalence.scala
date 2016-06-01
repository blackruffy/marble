package examples {

  object NumberEquivalence {
    import java.lang.Math._

    lazy val precision1 = 0.0001
    lazy val precision2 = java.lang.Math.ulp(1)

    // 小数点何桁まで比較した場合
    def equals1( a: Double, b: Double ): Boolean = abs( a - b ) < precision1

    // 上位何桁で比較した場合
    // aとbがほぼ同じ数字だったら割れば1付近になるはず。
    // precision = 1.00001(1 + java.lang.Math.ulp(1.0))のように１付近で比較する
    def equals2( a: Double, b: Double ): Boolean = {
      abs( a - b ) < java.lang.Math.ulp(a)
    }

    def perf = {
      val a = 32.567
      val b = 32.5669999

      for( j <- 1 to 10 ) {
        {
          val t1 = System.nanoTime
          for( i <- 1 to 10000 ) equals1( a, b )
          println( "1: %d".format( System.nanoTime - t1 ) )
        }

        {
          val t1 = System.nanoTime
          for( i <- 1 to 10000 ) equals2( a, b )
          println( "2: %d".format( System.nanoTime - t1 ) )
        }
      }
    }

    def main( args: Array[String] ) {

      {
        val a = 1234567890000000.0
        val b = 1234567889999999.0

        println( equals1( a, b ) )
        println( equals2( a, b ) )
      }

      {
        val a = 123456788999.0
        val b = 123456789000.0

        println( equals1( a, b ) )
        println( equals2( a, b ) )
      }

      {
        val a = 0.000000123456789
        val b = 0.000000123456432

        println( equals1( a, b ) )
        println( equals2( a, b ) )
      }

      {
        val a = 0.000000123456432
        val b = 0.000000123456789

        println( equals1( a, b ) )
        println( equals2( a, b ) )
      }

      {
        val a = -0.0001
        val b = 0.0001

        println( equals1( a, b ) )
        println( equals2( a, b ) )
      }

      perf
    }

  }

}
