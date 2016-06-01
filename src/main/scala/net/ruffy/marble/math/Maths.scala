package net.ruffy.marble.math {

  trait MathEnv {

    env: NumberEnv with VectorEnv with MatrixEnv with CoordinateSystemEnv =>

    object Math {

      lazy val _0 = mkNum( 0 )
      lazy val _1 = mkNum( 1 )
      lazy val _2 = mkNum( 2 )
      lazy val _3 = mkNum( 3 )
      lazy val _4 = mkNum( 4 )
      lazy val _5 = mkNum( 5 )

      //lazy val precision: Num = 0.0001
      //TODO: lazy val precision: Num = 1.0001
      //lazy val precision: Num = 1 + java.lang.Math.ulp(1.0)
      //TODO: lazy val precision: Num = 100f
      lazy val Pi: Num = math.Pi
      lazy val Pi_2: Num = Pi/2.0
      lazy val Pix2: Num = Pi*2.0

      def abs( a: Num ): Num = java.lang.Math.abs( a.toUnwrappedDouble )

      def pow( a: Num, b: Num ): Num = scala.math.pow( a.toUnwrappedDouble, b.toUnwrappedDouble )

      // 小数点何桁まで比較した場合
      //def equals( a: Num, b: Num, precision: Num ): Boolean = abs( a - b ) < precision

      // 上位何桁で比較した場合
      // aとbがほぼ同じ数字だったら割れば1付近になるはず。
      // precision = 1.00001(1 + java.lang.Math.ulp(1.0))のように１付近で比較する
      //def equals( a: Num, b: Num, precision: Num ): Boolean = abs( a / b ) < precision

      // TODO: より正確なアルゴリズム
      //    //def equals( a: Num, b: Num, precision: Num ): Boolean = {
      //    //  val ua = java.lang.Math.ulp(a)
      //    //  val ub = java.lang.Math.ulp(b)
      //    //  java.lang.Math.abs( a - b ) < (if(ua > ub) ua else ub)*precision
      //    //}

      def max( x: Num, y: Num ): (Num, Num) = if( x > y ) (x, y) else (y, x)

      def min( x: Num, y: Num ): (Num, Num) = if( x < y ) (x, y) else (y, x)

      //def equals( a: Num )( b: Num ): Boolean = equals( a, b, precision )
      def equals( a: Num )( b: Num ): Boolean = a == b

      def pow2( x: Num ): Num = x * x

      def random: Num = scala.math.random

      def sqrt( x: Num ): Num = math.sqrt( x.toUnwrappedDouble )

      def asin( x: Num ): Num = math.asin( x.toUnwrappedDouble )

      def acos( x: Num ): Num = math.acos( x.toUnwrappedDouble )

      def atan( x: Num ): Num = math.atan( x.toUnwrappedDouble )

      def atan2( x: Num, y: Num ): Num = math.atan2( x.toUnwrappedDouble, y.toUnwrappedDouble )

      def cos( x: Num ): Num = math.cos( x.toUnwrappedDouble )

      def sin( x: Num ): Num = math.sin( x.toUnwrappedDouble )

      def toDegrees( n: (Num, Num) ): (Num, Num) = {
        val (x, y) = n
        (x.toDegrees, y.toDegrees)
      }

      /**
        * 球の表面積を計算する。
        */
      def sphereSurfaceIntegral( r: Num, theta0: Num, theta1: Num, phi0: Num, phi1: Num ): Num = r * r * (phi1 - phi0) * (cos(theta0) - cos(theta1))

      /**
        * ベクトルnとベクトルxがなす角と、ベクトルn周りの回転角を求める。
        */
      def angles( n: Vector3, x: Vector3 ): (Num, Num) = CoordinateSystem.Axis3( n ).angles( x )

      def randomAngle( dth: Num ): (Num, Num) = {
        val s = Math.random
        val t = Math.random
        (s * dth, t * Math.Pix2)
      }

      /**
        * 球が与えられたとき、球の中心からベクトルvに対する天頂角thetaと方位角phiが示す球面上の点へ向かうベクトルを求める。
        */
      def createVector( v: Vector3, theta: Num, phi: Num ): Vector3 = {
        val a = CoordinateSystem.Axis3( v )
        val cos_phi = Math.cos( phi )
        val sin_phi = Math.sin( phi )
        val cos_theta = Math.cos( theta )
        val sin_theta = Math.sin( theta )
        a.nx * cos_phi * sin_theta + a.nz * sin_phi * sin_theta + a.ny * cos_theta
      }

      def unwrap( xy: (Num, Num) ): (NumType, NumType) = (xy._1.underlying, xy._2.underlying)

    }

  }

}
