package net.ruffy.marble.graphics3d {

  import net.ruffy.marble.math.{ Vector3, Math }
  import net.ruffy.marble.graphics2d.{ Color, Image }
  import shapes.{ BoundingSphere }
  import net.ruffy.marble.graphics3d.{ Octree => OctreeB }
  import java.nio.ByteBuffer
  import Math._

  trait PhotonMap[A] {

    val underlying: A

    def update( a: Vector3, b: Color ): Unit
    def foreach( f: (Vector3, Color) => Unit ): Unit
    def write( fname: String ): Unit
    def write( toScreen: Vector3 => (Int, Int, Int), image: Image[Color], ext: String, fname: String ): Unit
    def sum( c: Vector3, n: Vector3, a: Float, b: Float ): Color
    def sum( b: BoundingSphere ): Color
  }

  object PhotonMap {

    class Octree(
      xmin: Float, xmax: Float,
      ymin: Float, ymax: Float,
      zmin: Float, zmax: Float,
      capacity: Int,
      parent: Option[Octree]
    ) extends OctreeB[Color, Octree]( xmin, xmax, ymin, ymax, zmin, zmax, capacity, None ) with PhotonMap[Octree] {

      val underlying = this

      private var _sum: Color = Color.Black

      protected override def onUpdate( b: Color ): Unit = _sum = _sum + b

      def update( a: Vector3, b: Color ): Unit = add( a, b )

      protected def create(
        xmin: Float, xmax: Float,
        ymin: Float, ymax: Float,
        zmin: Float, zmax: Float,
        capacity: Int,
        parent: Option[Octree]
      ): Octree = new Octree( xmin, xmax, ymin, ymax, zmin, zmax, capacity, parent )

      def filter( b: BoundingSphere, f: (Vector3, Color) => Unit ): Unit = filterChildren( b, {
        case (2, o) => o foreach f
        case (_, o) => o foreach { ( p, q ) => if( b.inside_?( p ) ) f( p, q ) }
      } )

      /**
        * c: 中心
        * n: 短軸の向き
        */
      def sum( c: Vector3, n: Vector3, a: Float, b: Float ): Color = {
        var col: Color = Color.Black
        val a2 = a * a
        val b2 = b * b
        val ab = a * b
        filter( BoundingSphere(c, a), ( p, q ) => {
          val v = c -> p
          val theta = Pi_2 - (n angle v)
          val bcos = b * cos( theta )
          val asin = a * sin( theta )
          val r = ab / sqrt( bcos * bcos + asin * asin )
          val d = v.abs
          //println("***** %s, %f, %f, %f, %f, %f".format(p, a, d, r, bcos, asin))
          if( d < r ) col = col + q
        } )
        col
      }

      def sum( b: BoundingSphere ): Color = {
        var col: Color = Color.Black
        filterChildren( b, {
          case (2, o) => col = col + o._sum
          case (_, o) => o.foreach { ( p, q ) =>
            if( b.inside_?( p ) ) col = col + q
          }
        } )
        col
      }

      def write( fname: String ): Unit = write( fname, 16, { (buf, c) =>
        buf.putFloat( c.alpha )
        buf.putFloat( c.red )
        buf.putFloat( c.green )
        buf.putFloat( c.blue )
      } )

      def write( toScreen: Vector3 => (Int, Int, Int), image: Image[Color], ext: String, fname: String ): Unit = {
        foreach { (p, c) =>
          val (x, y, z) = toScreen( p )
          image.update( x, y, c )
        }
        
        image.write( ext, fname )
      }

    }

    object Octree {

      def apply(
        xmin: Float, xmax: Float,
        ymin: Float, ymax: Float,
        zmin: Float, zmax: Float,
        capacity: Int = 20
      ): PhotonMap[Octree] = new PhotonMap.Octree( xmin, xmax, ymin, ymax, zmin, zmax, capacity, None )

      def apply( sphere: BoundingSphere ): PhotonMap[Octree] = PhotonMap.Octree(
        sphere.center.x - sphere.radius, sphere.center.x + sphere.radius,
        sphere.center.y - sphere.radius, sphere.center.y + sphere.radius,
        sphere.center.z - sphere.radius, sphere.center.z + sphere.radius
      )

      def apply( fname: String ): PhotonMap[Octree] = OctreeB[Color, PhotonMap.Octree]( fname, { b: ByteBuffer => Color(
        b.getFloat,
        b.getFloat,
        b.getFloat,
        b.getFloat
      ) }, (x0, x1, y0, y1, z0, z1, cap) => new PhotonMap.Octree(x0, x1, y0, y1, z0, z1, cap, None) )
      
    }

//    def apply( self: Octree[Color] ): PhotonMap[Octree[Color]] = new PhotonMap[Octree[Color]] {
//
//      val underlying = self
//
//      def update( a: Vector3, b: Color ): Unit = self.update( a, b )
//
//      def foreach( f: (Vector3, Color) => Unit ): Unit = self.foreach( f )
//
//      def write( fname: String ): Unit = self.write( fname, 16, { (buf, c) =>
//        buf.putFloat( c.alpha )
//        buf.putFloat( c.red )
//        buf.putFloat( c.green )
//        buf.putFloat( c.blue )
//      } )
//
//      def write( toScreen: Vector3 => (Int, Int, Int), image: Image[Color], ext: String, fname: String ): Unit = {
//        foreach { (p, c) =>
//          val (x, y, z) = toScreen( p )
//          image.update( x, y, c )
//        }
//        
//        image.write( ext, fname )
//      }
//
//      def filter( b: BoundingSphere, f: (Vector3, Color) => Unit ): Unit = self.filter( b, f )
//
//    }



  }

}
