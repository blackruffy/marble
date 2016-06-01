package net.ruffy.marble.math {

  trait MatrixEnv {

    env: MathEnv
        with NumberEnv
        with VectorEnv
        with MatrixEnv =>

    abstract class Matrix[A <: Matrix[A]]( val elems: Array[Array[Num]] ) {

      lazy val nrows = elems.size

      lazy val ncols = elems.head.size

      protected def create( elems: Array[Array[Num]] ): A

      def apply( i: Int, j: Int ) = elems( i )( j )

      def + ( m: A ): A = create( ( elems, m.elems ).zipped.map { (xs, ys) =>
        (xs, ys).zipped.map { (x, y) => x + y }
      } )

      def - ( m: A ): A = create( ( elems, m.elems ).zipped.map { (xs, ys) =>
        (xs, ys).zipped.map { (x, y) => x - y }
      } )

      protected def mult( xs: Array[Array[Num]] ): Array[Array[Num]] = {
        val ys = new Array[Array[Num]]( nrows )
        val nc = xs.head.size
        for( k <- 0 until nrows ) {
          ys( k ) = new Array[Num]( nc )
          for( i <- 0 until nc ) {
            var x = Math._0
            for( j <- 0 until ncols ) {
              x = x + elems( k )( j ) * xs( j )( i )
            }
            ys( k )( i ) = x
          }
        }
        ys
      }

      override def equals( x: Any ) = x match {
        case m: A => ~==( m )
        case _ => false
      }

      def * ( x: Num ): A = create( elems.map { _.map { _ * x } } )

      def ~== ( m: A ): Boolean = ( elems, m.elems ).zipped.forall { (xs, ys) =>
        (xs, ys).zipped.forall { (x, y) => Math.equals(x)(y) }
      }

      override def toString = elems.map( _.mkString(", ") ).mkString("\n")
    }

    class Matrix4x4(
      a00: Num, a01: Num, a02: Num, a03: Num,
      a10: Num, a11: Num, a12: Num, a13: Num,
      a20: Num, a21: Num, a22: Num, a23: Num,
      a30: Num, a31: Num, a32: Num, a33: Num
    ) extends Matrix[Matrix4x4]( Array(
      Array( a00, a01, a02, a03 ),
      Array( a10, a11, a12, a13 ),
      Array( a20, a21, a22, a23 ),
      Array( a30, a31, a32, a33 )
    ) ) {

      lazy val determinant: Num = {
        a00*a11*a22*a33 + a00*a12*a23*a31 + a00*a13*a21*a32 +
        a01*a10*a23*a32 + a01*a12*a20*a33 + a01*a13*a22*a30 +
        a02*a10*a21*a33 + a02*a11*a23*a30 + a02*a13*a20*a31 +
        a03*a10*a22*a31 + a03*a11*a20*a32 + a03*a12*a21*a30 -
        a00*a11*a23*a32 - a00*a12*a21*a33 - a00*a13*a22*a31 -
        a01*a10*a22*a33 - a01*a12*a23*a30 - a01*a13*a20*a32 -
        a02*a10*a23*a31 - a02*a11*a20*a33 - a02*a13*a21*a30 -
        a03*a10*a21*a32 - a03*a11*a22*a30 - a03*a12*a20*a31
      }

      protected def create( a: Array[Array[Num]] ) = Matrix4x4(
        a( 0 )( 0 ), a( 0 )( 1 ), a( 0 )( 2 ), a( 0 )( 3 ),
        a( 1 )( 0 ), a( 1 )( 1 ), a( 1 )( 2 ), a( 1 )( 3 ),
        a( 2 )( 0 ), a( 2 )( 1 ), a( 2 )( 2 ), a( 2 )( 3 ),
        a( 3 )( 0 ), a( 3 )( 1 ), a( 3 )( 2 ), a( 3 )( 3 )
      )

      def * ( m: Matrix4x4 ): Matrix4x4 = create( mult( m.elems ) )

      def * ( v: Vector3 ): Vector3 = {
        val a = mult( Array( Array( v( 0 ) ), Array( v( 1 ) ), Array( v( 2 ) ), Array( Math._1 ) ) )
        Vector3( a( 0 )( 0 ), a( 1 )( 0 ), a( 2 )( 0 ) )
      }

      def inverse: Matrix4x4 = {
        val b00 = a11*a22*a33 + a12*a23*a31 + a13*a21*a32 - a11*a23*a32 - a12*a21*a33 - a13*a22*a31
        val b01 = a01*a23*a32 + a02*a21*a33 + a03*a22*a31 - a01*a22*a33 - a02*a23*a31 - a03*a21*a32
        val b02 = a01*a12*a33 + a02*a13*a31 + a03*a11*a32 - a01*a13*a32 - a02*a11*a33 - a03*a12*a31
        val b03 = a01*a13*a22 + a02*a11*a23 + a03*a12*a21 - a01*a12*a23 - a02*a13*a21 - a03*a11*a22
        val b10 = a10*a23*a32 + a12*a20*a33 + a13*a22*a30 - a10*a22*a33 - a12*a23*a30 - a13*a20*a32
        val b11 = a00*a22*a33 + a02*a23*a30 + a03*a20*a32 - a00*a23*a32 - a02*a20*a33 - a03*a22*a30
        val b12 = a00*a13*a32 + a02*a10*a33 + a03*a12*a30 - a00*a12*a33 - a02*a13*a30 - a03*a10*a32
        val b13 = a00*a12*a23 + a02*a13*a20 + a03*a10*a22 - a00*a13*a22 - a02*a10*a23 - a03*a12*a20
        val b20 = a10*a21*a33 + a11*a23*a30 + a13*a20*a31 - a10*a23*a31 - a11*a20*a33 - a13*a21*a30
        val b21 = a00*a23*a31 + a01*a20*a33 + a03*a21*a30 - a00*a21*a33 - a01*a23*a30 - a03*a20*a31
        val b22 = a00*a11*a33 + a01*a13*a30 + a03*a10*a31 - a00*a13*a31 - a01*a10*a33 - a03*a11*a30
        val b23 = a00*a13*a21 + a01*a10*a23 + a03*a11*a20 - a00*a11*a23 - a01*a13*a20 - a03*a10*a21
        val b30 = a10*a22*a31 + a11*a20*a32 + a12*a21*a30 - a10*a21*a32 - a11*a22*a30 - a12*a20*a31
        val b31 = a00*a21*a32 + a01*a22*a30 + a02*a20*a31 - a00*a22*a31 - a01*a20*a32 - a02*a21*a30
        val b32 = a00*a12*a31 + a01*a10*a32 + a02*a11*a30 - a00*a11*a32 - a01*a12*a30 - a02*a10*a31
        val b33 = a00*a11*a22 + a01*a12*a20 + a02*a10*a21 - a00*a12*a21 - a01*a10*a22 - a02*a11*a20
        Matrix4x4(
          b00, b01, b02, b03,
          b10, b11, b12, b13,
          b20, b21, b22, b23,
          b30, b31, b32, b33
        ) * ( Math._1/determinant )
      }

    }

    object Matrix4x4 {

      def apply(
        a00: Num, a01: Num, a02: Num, a03: Num,
        a10: Num, a11: Num, a12: Num, a13: Num,
        a20: Num, a21: Num, a22: Num, a23: Num,
        a30: Num, a31: Num, a32: Num, a33: Num
      ): Matrix4x4 = new Matrix4x4(
        a00, a01, a02, a03,
        a10, a11, a12, a13,
        a20, a21, a22, a23,
        a30, a31, a32, a33
      )

      lazy val unit: Matrix4x4 = apply(
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
      )

      lazy val empty: Matrix4x4 = apply(
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0
      )

      /**
        * create a matrix which rotate theta radians about v axis
        */
      def rotation( v: Vector3, theta: Num ): Matrix4x4 = {
        val n = v.normalize
        val nx = n( 0 )
        val ny = n( 1 )
        val nz = n( 2 )
        val cos: Num = Math.cos( theta )
        val sin: Num = Math.sin( theta )
        Matrix4x4(
          nx*nx*(Math._1-cos)+   cos, nx*ny*(Math._1-cos)-nz*sin, nz*nx*(Math._1-cos)+ny*sin, 0,
          nx*ny*(Math._1-cos)+nz*sin, ny*ny*(Math._1-cos)+   cos, ny*nz*(Math._1-cos)-nx*sin, 0,
          nz*nx*(Math._1-cos)-ny*sin, ny*nz*(Math._1-cos)+nx*sin, nz*nz*(Math._1-cos)+   cos, 0,
          Math._0,                    Math._0,                    Math._0,                    Math._1
        )
      }

      /**
        * create a matrix which rotate theta radians about x axis
        */
      def rotationX( theta: Num ): Matrix4x4 = {
        val cos = Math.cos( theta )
        val sin = Math.sin( theta )
        Matrix4x4(
          1,   0,    0, 0,
          0, cos, -sin, 0,
          0, sin,  cos, 0,
          0,   0,    0, 1
        )
      }

      /**
        * create a matrix which rotate theta radians about y axis
        */
      def rotationY( theta: Num ): Matrix4x4 = {
        val cos = Math.cos( theta )
        val sin = Math.sin( theta )
        Matrix4x4(
          cos,   0,    sin, 0,
          0,   1,      0, 0,
          -sin,   0,    cos, 0,
          0,   0,      0, 1
        )
      }

      /**
        * create a matrix which rotate theta radians about z axis
        */
      def rotationZ( theta: Num ): Matrix4x4 = {
        val cos = Math.cos( theta )
        val sin = Math.sin( theta )
        Matrix4x4(
          cos, -sin, 0, 0,
          sin,  cos, 0, 0,
          0,    0, 1, 0,
          0,    0, 0, 1
        )
      }

      def scale( x: Num, y: Num, z: Num ): Matrix4x4 = Matrix4x4(
        x, 0, 0, 0,
        0, y, 0, 0,
        0, 0, z, 0,
        0, 0, 0, 1
      )

      def translate( x: Num, y: Num, z: Num ): Matrix4x4 = Matrix4x4(
        1, 0, 0, x,
        0, 1, 0, y,
        0, 0, 1, z,
        0, 0, 0, 1
      )

      /**
        * Create a matrix to transform a point on a coordinate system, which is defined by o, nx and nz, to current coordinate system.
        * @param o The origin in current coordinate system.
        * @param nx The x axis in current coordinate system.
        */
      def transform( o: Vector3, _nx: Vector3, _nz: Vector3 ): Matrix4x4 = {

        val nx = _nx.normalize
        val nz = _nz.normalize

        val rnz = Vector3.Nz x nz
        val anz = Vector3.Nz angle nz
        val r1 = Matrix4x4.rotation( rnz, anz )

        val x1 = r1 * Vector3.Nx
        val anx = x1 angle nx
        val r2 = Matrix4x4.rotation( nz, anx )

        Matrix4x4.translate( o.x, o.y, o.z ) * r2 * r1
      }

    }

  }
}
