package net.ruffy.marble.math {

  trait VectorEnv {

    env: MathEnv
        with NumberEnv =>

    import Math._

    abstract class Vector[A <: Vector[A]]( val elems: Array[Num], val normalized_? : Boolean ) {

      self: A =>

      lazy val abs2: Num = self * self
      lazy val abs: Num = sqrt( abs2 )
      lazy val normalize = if( normalized_? ) this else create( elems.map { _ / abs }, true )

      protected def create( xs: Array[Num], normalized_? : Boolean ): A

      protected def create( xs: Array[Num] ): A = create( xs, false )

      def apply( i: Int ): Num = elems( i )

      lazy val unary_- : A = this * env.mkNum(-1)

      def + ( v: A ) : A = create( ( elems, v.elems ).zipped.map { (x, y) => x + y } )

      def - ( v: A ) : A = create( ( elems, v.elems ).zipped.map { (x, y) => x - y } )

      def * ( v: A ) : Num = ( elems, v.elems ).zipped.foldLeft( _0 ) { case (v, (x, y)) => v + x * y }

      def * ( x: Num ) : A = create( elems.map { _ * x } )

      def / ( x: Num ) : A = create( elems.map { _ / x } )

      def mult ( v: A ) : A = create( ( elems, v.elems ).zipped.map { (x, y) => x * y } )

      def angle( v: A ): Num =
        if( equals( v ) ) _0
        else if( v.isEmpty ) _0
        else if( isEmpty ) _0
        else acos( ( this * v )  / ( abs * v.abs ) )

      def interpolate( v: A )( a: Num, b: Num ): A = create( ( elems, v.elems ).zipped.map { (p, q) => (b*p + a*q)/(a + b) } )

      def normal( v: A ): A = this x v

      def x ( v: A ): A = (elems, v.elems) match {
        case (Array(a, b, c), Array(x, y, z) ) => create( Array( b*z - c*y, c*x - a*z, a*y - b*x ) )
        case _ => throw new Exception("unsupported number of elements")
      }

      def reflect( n: A ): A = this + ( n * ( this * n ) * env.mkNum(-2) )

      def refract( n: A, eta1: Num, eta2: Num ): A = {
        val l = this * env.mkNum(-1)
        val ln = l * n
        val eta = eta1/eta2
        (n * sqrt(_1 - eta * eta * (_1 - ln*ln)) + (l - n * ln) * eta) * env.mkNum(-1)
      }

      lazy val isEmpty = elems.forall( x => Math.equals(_0)(x) )

      override def equals( a: Any ) = a match {
        case v: A => ~==( v )
        case _ => false
      }

      def ~== ( v: A ): Boolean = ( elems, v.elems ).zipped.forall { (x, y) => Math.equals(x)(y) }

      def -> ( v: A ): A = v - this

      override def toString: String = "(%s)".format( elems.mkString(",") )
    }

    class Vector2( val x: Num, val y: Num, normalized_? : Boolean ) extends Vector[Vector2]( Array[Num]( x, y ), normalized_? ) {

      protected def create( xs: Array[Num], normalized_? : Boolean ) = xs match {
        case Array( x, y ) => Vector2( x, y, normalized_? )
        case _ => throw new Exception("unexpected number of elements: %d".format( xs.size ))
      }

      def toVector3 = Vector3( x, y, 0, normalized_? )

    }

    object Vector2 {
      def apply( x: Num, y: Num, normalized_? : Boolean ): Vector2 = new Vector2( x, y, normalized_? )
      def apply( x: Num, y: Num ): Vector2 = new Vector2( x, y, false )
      def unapply( v: Vector2 ): Option[(Num, Num)] = Some( v.x, v.y )
    }

    class Vector3( val x: Num, val y: Num, val z: Num, normalized_? : Boolean ) extends Vector[Vector3]( Array[Num]( x, y, z ), normalized_? ) {

      protected def create( xs: Array[Num], normalized_? : Boolean ) = xs match {
        case Array( x, y, z ) => Vector3( x, y, z, normalized_? )
        case _ => throw new Exception("unexpected number of elements: %d".format( xs.size ))
      }

      def toVector4 = Vector4( x, y, z, _1, normalized_? )

      def toPolar: (Num, Num, Num) = {
        val r = abs
        val x0 = Math.equals(x, _0)
        val z0 = Math.equals(z, _0)

        val phi = if( x0 && z0 ) _0
        else if( x0 && z > _0 ) Pi_2
        else if( x0 && z < _0 ) Pi + Pi_2
        else if( z0 && x < _0 ) Pi
        else if( z0 && x > _0 ) _0
        else if( z > _0 ) atan2( z, x )
        else Pix2 + atan2( z, x )

        val theta = if( Math.equals(r, _0) ) _0 else acos( y/r )

        ( r, theta, phi )
      }

    }

    object Vector3 {

      val O  = apply( 0, 0, 0 )
      val Nx = apply( 1, 0, 0 )
      val Ny = apply( 0, 1, 0 )
      val Nz = apply( 0, 0, 1 )

      def fromPolar( r: Num, theta: Num, phi: Num ) = Vector3( r*sin( theta )*cos( phi ), r*cos( theta ), r*sin( theta )*sin( phi ) )

      def apply( x: Num, y: Num, z: Num, normalized_? : Boolean ): Vector3 = new Vector3( x, y, z, normalized_? )

      def apply( x: Num, y: Num, z: Num ): Vector3 = apply( x, y, z, false )

      def unapply( v: Vector3 ): Option[(Num, Num, Num)] = Some( (v.x, v.y, v.z) )

      def toStringAsDegrees( v: (Num, Num, Num) ): String = v match {
        case (r, t, p) => (r, t.toDegrees, p.toDegrees).toString
      }
    }

    class Vector4( val x: Num, val y: Num, val z: Num, val t: Num, normalized_? : Boolean ) extends Vector[Vector4]( Array( x, y, z, t ), normalized_? ) {

      protected def create( xs: Array[Num], normalized_? : Boolean ) = xs match {
        case Array( x, y, z, t ) => Vector4( x, y, z, t, normalized_? )
        case _ => throw new Exception("unexpected number of elements: %d".format( xs.size ))
      }

      def toVector3 = Vector3( x, y, z, normalized_? )
    }

    object Vector4 {

      def apply( x: Num, y: Num, z: Num, t: Num, normalized_? : Boolean ): Vector4 = new Vector4( x, y, z, t, normalized_? )

      def apply( x: Num, y: Num, z: Num, t: Num ): Vector4 = new Vector4( x, y, z, t, false )

      def unapply( v: Vector4 ): Option[(Num, Num, Num, Num)] = Some( (v.x, v.y, v.z, v.t) )

    }

  }

}

