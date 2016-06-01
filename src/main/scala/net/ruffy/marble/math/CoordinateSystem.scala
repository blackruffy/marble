package net.ruffy.marble.math {

  trait CoordinateSystemEnv {

    env: MathEnv with VectorEnv with MatrixEnv with NumberEnv =>

    class CoordinateSystem( val origin: Vector3, val axis: CoordinateSystem.Axis3 ) {
      lazy val nx = axis.nx
      lazy val ny = axis.ny
      lazy val nz = axis.nz

      protected lazy val trans = Matrix4x4.transform( origin, nx, nz )
      protected lazy val itrans = trans.inverse

      /**
        * 点pをこの座標系の表示に変換する。
        */
      def transform( p: Vector3 ): Vector3 = itrans * p

      /**
        * この座標系表示の点pを元の座標系表示に変換する。
        */
      def restore( p: Vector3 ): Vector3 = trans * p
    }

    object CoordinateSystem {

      class Axis3( x: Vector3, y: Vector3, z: Vector3 ) {
        val nx = x.normalize
        val ny = y.normalize
        val nz = z.normalize

        /**
          * y軸とベクトルvがなす角と、y軸周りの方位角を求める。
          */
        def angles( v: Vector3 ): (Num, Num) = {
          
          val theta = ny angle v

          val vny = ny * (v.abs * Math.cos( theta ))
          val vnxz = vny -> v
          val phi = nx angle vnxz
          
          //val uz = x x ny
          //val x2 = Matrix4x4.rotation( -uz, Math.Pi_2 - theta ) * x
          //val phi = x2 angle nx
          
          (theta, phi)
        }

      }

      object Axis3 {

        def apply( x: Vector3, y: Vector3, z: Vector3 ): Axis3 = new Axis3( x, y, z )

        /**
          * 現座標系のy軸と新座標系のy軸を表す単位ベクトルnyがなす平面に垂直な方向をz軸とする座標系を生成する。
          * 原点は、現座標系と同じ
          */
        def apply( ny: Vector3 ): Axis3 = {
          val vy = ny.normalize
          val (vx, vz) = if( vy == Vector3.Ny ) {
            (Vector3.Nx, Vector3.Nz)
          } else {
            val vz = vy x Vector3.Ny
            (vz x vy, vz)
          }
          new Axis3( vx, vy, vz )
        }

        def unapply( a: Axis3 ): Option[(Vector3, Vector3, Vector3)] = Some( (a.nx, a.ny, a.nz) )

      }

      def apply( o: Vector3, ny: Vector3 ): CoordinateSystem = new CoordinateSystem( o, Axis3( ny ) )

      def unapply( c: CoordinateSystem ): Option[(Vector3, Axis3)] = Some( ( c.origin, c.axis ) )

    }

  }

}
