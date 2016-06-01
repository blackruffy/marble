package net.ruffy.marble.math {

  abstract class Vector[A <: Vector[A]]( val elems: Array[Float], val normalized_? : Boolean ) {

    self: A =>

    import WrappedFloat.toWrappedFloat

    lazy val abs2: Float = self * self
    lazy val abs: Float = Math.sqrt( abs2 )
    lazy val normalize = if( normalized_? ) this else create( elems.map { _ / abs }, true )

    protected def create( xs: Array[Float], normalized_? : Boolean ): A

    protected def create( xs: Array[Float] ): A = create( xs, false )

    def apply( i: Int ): Float = elems( i )

    lazy val unary_- : A = this * (-1)

    def + ( v: A ) : A = create( ( elems, v.elems ).zipped.map { (x, y) => x + y } )

    def - ( v: A ) : A = create( ( elems, v.elems ).zipped.map { (x, y) => x - y } )

    def * ( v: A ) : Float = ( elems, v.elems ).zipped.foldLeft( 0.0f ) { case (v, (x, y)) => v + x * y }

    def * ( x: Float ) : A = create( elems.map { _ * x } )

    def / ( x: Float ) : A = create( elems.map { _ / x } )

    def / ( x: Int ) : A = create( elems.map { _ / x } )

    def mult ( v: A ) : A = create( ( elems, v.elems ).zipped.map { (x, y) => x * y } )

    def angle( v: A ): Float =
      if( equals( v ) ) 0f
      else if( v.isEmpty ) 0f
      else if( isEmpty ) 0f
      else Math.acos( ( this * v )  / ( abs * v.abs ) )

    def interpolate( v: A )( a: Float, b: Float ): A = create( ( elems, v.elems ).zipped.map { (p, q) => (b*p + a*q)/(a + b) } )

    def normal( v: A ): A = this x v

    def x ( v: A ): A = (elems, v.elems) match {
      case (Array(a, b, c), Array(x, y, z) ) => create( Array( b*z - c*y, c*x - a*z, a*y - b*x ) )
      case _ => throw new Exception("unsupported number of elements")
    }

    def reflect( n: A ): A = this + ( n * ( this * n ) * -2f )

    def refract( n: A, eta1: Float, eta2: Float ): A = {
      val l = this * (-1)
      val ln = l * n
      val eta = eta1/eta2
      (n * Math.sqrt(1 - eta * eta * (1 - ln*ln)) + (l - n * ln) * eta) * (-1)
    }

    lazy val isEmpty = elems.forall( Math.equals(0f) )

    override def equals( a: Any ) = a match {
      case v: A => ~==( v )
      case _ => false
    }

    def ~== ( v: A ): Boolean = ( elems, v.elems ).zipped.forall { (x, y) => Math.equals(x)(y) }

    def -> ( v: A ): A = v - this

    override def toString: String = "(%s)".format( elems.mkString(",") )
  }

  class Vector3( val x: Float, val y: Float, val z: Float, normalized_? : Boolean ) extends Vector[Vector3]( Array( x, y, z ), normalized_? ) {

    protected def create( xs: Array[Float], normalized_? : Boolean ) = xs match {
      case Array( x, y, z ) => Vector3( x, y, z, normalized_? )
      case _ => throw new Exception("unexpected number of elements: %d".format( xs.size ))
    }

    def toVector4 = Vector4( x, y, z, 1, normalized_? )

    def toPolar: (Float, Float, Float) = {
      val r = abs
      val x0 = WrappedFloat(x) == 0f
      val z0 = WrappedFloat(z) == 0f

      val phi = if( x0 && z0 ) 0f
      else if( x0 && z > 0 ) Math.Pi_2
      else if( x0 && z < 0 ) Math.Pi + Math.Pi_2
      else if( z0 && x < 0 ) Math.Pi
      else if( z0 && x > 0 ) 0f
      else if( z > 0 ) Math.atan2( z, x )
      else Math.Pix2 + Math.atan2( z, x )

      val theta = if( WrappedFloat(r) == 0f ) 0 else Math.acos( y/r )

      ( r, theta, phi )
    }

  }

  object Vector3 {
    import Math._

    val O  = Vector3( 0, 0, 0 )
    val Nx = Vector3( 1, 0, 0 )
    val Ny = Vector3( 0, 1, 0 )
    val Nz = Vector3( 0, 0, 1 )

    def fromPolar( r: Float, theta: Float, phi: Float ) = Vector3( r*sin( theta )*cos( phi ), r*cos( theta ), r*sin( theta )*sin( phi ) )

    def apply( x: Float, y: Float, z: Float, normalized_? : Boolean ): Vector3 = new Vector3( x, y, z, normalized_? )

    def apply( x: Float, y: Float, z: Float ): Vector3 = new Vector3( x, y, z, false )

    def unapply( v: Vector3 ): Option[(Float, Float, Float)] = Some( (v.x, v.y, v.z) )

    def toStringAsDegrees( v: (Float, Float, Float) ): String = v match {
      case (r, t, p) => (r, t.toDegrees, p.toDegrees).toString
    }
  }

  class Vector4( val x: Float, val y: Float, val z: Float, val t: Float, normalized_? : Boolean ) extends Vector[Vector4]( Array( x, y, z, t ), normalized_? ) {

    protected def create( xs: Array[Float], normalized_? : Boolean ) = xs match {
      case Array( x, y, z, t ) => Vector4( x, y, z, t, normalized_? )
      case _ => throw new Exception("unexpected number of elements: %d".format( xs.size ))
    }

    def toVector3 = Vector3( x, y, z, normalized_? )
  }

  object Vector4 {

    def apply( x: Float, y: Float, z: Float, t: Float, normalized_? : Boolean ): Vector4 = new Vector4( x, y, z, t, normalized_? )

    def apply( x: Float, y: Float, z: Float, t: Float ): Vector4 = new Vector4( x, y, z, t, false )

    def unapply( v: Vector4 ): Option[(Float, Float, Float, Float)] = Some( (v.x, v.y, v.z, v.t) )

  }

}

