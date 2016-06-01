package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math._
  import net.ruffy.marble.graphics2d._
  import net.ruffy.marble.graphics3d._
  import WrappedFloat._
  import Material.Medium

  class Triangle(
    val p0: Vector3, val n0: Vector3,
    val p1: Vector3, val n1: Vector3,
    val p2: Vector3, val n2: Vector3,
    val material: Material
  ) extends Polygon {

    lazy val points = List( p0, p1, p2 )
    lazy val normals = List( n0, n1, n2 )
    lazy val normal = Triangle.normal( p0, p1, p2 )
    private lazy val List((q0, m0), (q1, m1), (q2, m2)) = List((p0, n0), (p1, n1), (p2, n2)).sortWith { case ((a, u), (b, v)) => a.y < b.y }
    lazy val center = (p0 + p1 + p2) / 3
    lazy val boundingSphere = BoundingSphere( center, points.foldLeft( 0f ) { ( max, p ) =>
      val r = (center -> p).abs
      if( r > max ) r else max
    } )
    lazy val v01 = p0 -> p1
    lazy val v02 = p0 -> p2

    /**
      * 点q3の法線を各頂点の法線から求める。
      */
    def normal( q3: Vector3 ): Vector3 = if( normals.isEmpty ) normal else Triangle.getNormal( q0, m0, q1, m1, q2, m2, q3 )

    /**
      * 点pがこの三角形の内側(法線と反対側)にあるか判定する。
      */
    def inside_?( p: Vector3 ): Boolean = ((points.head -> p) angle normal) > Math.Pi_2

    def create( points: List[Vector3], normals: List[Vector3], material: Material ) = new Triangle(
      points(0), normals(0),
      points(1), normals(1),
      points(2), normals(2),
      material
    )

    def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = if( normal.z < 0 ) {
      val screen = camera.screen
      val (x0, y0, z0) = screen.pixel( camera.toScreen( p0 ) )
      val (x1, y1, z1) = screen.pixel( camera.toScreen( p1 ) )
      val (x2, y2, z2) = screen.pixel( camera.toScreen( p2 ) )
      val mtype = if( Math.equals(material.surface.emittance)(1f) ) Material.Emittance else Material.Diffusivity
      val color = lights.foldLeft[Color]( Color.Black ) { case ( d, l ) => d + l.color( normal, center, material, mtype ) }
      g.fillTriangle( x0, y0, x1, y1, x2, y2, color.toInt )
    }

    def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ): Unit = {
      val screen = camera.screen
      val (x0, y0, z0) = screen.pixel( camera.toScreen( p0 ) )
      val (x1, y1, z1) = screen.pixel( camera.toScreen( p1 ) )
      val (x2, y2, z2) = screen.pixel( camera.toScreen( p2 ) )
      val mtype = if( Math.equals(material.surface.emittance)(1f) ) Material.Emittance else Material.Diffusivity
      val (c0, c1, c2) = lights.foldLeft[(Color, Color, Color)]( (Color.Black, Color.Black, Color.Black) ) { case ( (d0, d1, d2), l ) => (
        d0 + l.color( n0, p0, material, mtype ),
        d1 + l.color( n1, p1, material, mtype ),
        d2 + l.color( n2, p2, material, mtype )
      ) }
      g.fillTriangleZ(
        x0, y0, z0, c0.toInt,
        x1, y1, z1, c1.toInt,
        x2, y2, z2, c2.toInt
      )
    }

    /**
      * Calculate a intersection with a ray.
      * @return u, v and t that the intersection P = u*AB + v*AC and P = t * ray.
      */
    def calcIntersection( ray: Photon ): ( Float, Float, Float ) = {
      val e1 = v01 // p0 -> p1
      val e2 = v02 // p0 -> p2
      val s  = p0 -> ray.origin
      val p  = ray.direction x e2
      val q  = s x e1
      val c  = 1 / (p * e1)
      val u  = c * ( p * s )
      val v  = c * ( q * ray.direction )
      val t  = c * ( q * e2 )
      (t, u, v)
    }

    def intersect_?( t: Float, u: Float, v: Float ): Boolean = {
      val _t = if( Math.equals(t)(0f) ) 0f else t
      val _u = if( Math.equals(u)(0f) ) 0f else u
      val _v = if( Math.equals(v)(0f) ) 0f else v
      if( _u < 0 || _u > 1.0 ) false
      else if( _v < 0 || _u + _v > 1.0 ) false
      else if( _t <= 0 ) false
      else true
    }

    def getIntersection( ray: Photon ): Option[Intersection] = {
      val (t, u, v) = calcIntersection( ray )
      if( intersect_?( t, u, v ) ) {
        val ip = ray.origin + ray.direction * t
        Some( Intersection( ray, t, this, ip, normal(ip) ) )
      } else None
    }

    def randomPosition: Vector3 = {
      val v1 = v01 * Math.random
      val v2 = v02 * Math.random
      val s = Math.random
      val v = v1 * s + v2 * (1 - s)
      p0 + v
    }

    def createPhoton( color: Color ): Photon = Photon.createRandomPhoton( randomPosition, normal, color, Medium.Default, None )

  }

  object Triangle {

    def normal( p0: Vector3, p1: Vector3, p2: Vector3 ) = ( ( p0 -> p1 ) x ( p0 -> p2 ) ).normalize

    /**
      * Calculate a normal line of a point in the triangle given by p0, p1 and p2.
      * @param p0 A point to make a triangle.
      * @param p1 A point to make a triangle.
      * @param p2 A point to make a triangle.
      */
    private def getNormal( p0: Vector3, n0: Vector3, p1: Vector3, n1: Vector3, p2: Vector3, n2: Vector3, p3: Vector3 ): Vector3 = {

      val v03 = p0 -> p3
      val v01 = p0 -> p1
      val v12 = p1 -> p2

      val s = (v03 x v01).abs / (v03 x v12).abs
      val v4 = v12 * s + p1
      val n4 = n1.interpolate( n2 )( s, 1 - s )

      val t = v03.abs / (v4 - p0).abs
      n0.interpolate( n4 )( t, 1 - t ).normalize
    }

    def apply( p0: Vector3, p1: Vector3, p2: Vector3, material: Material ): Triangle = {
      val n = normal( p0, p1, p2 )
      new Triangle( p0, n, p1, n, p2, n, material )
    }

    def apply( p0: Vector3, p1: Vector3, p2: Vector3 ): Triangle = apply( p0, p1, p2, Material.Default )

    def apply(
      p0: Vector3, n0: Vector3,
      p1: Vector3, n1: Vector3,
      p2: Vector3, n2: Vector3,
      material: Material
    ): Triangle = new Triangle( p0, n0, p1, n1, p2, n2, material )

    def apply(
      p0: Vector3, n0: Vector3,
      p1: Vector3, n1: Vector3,
      p2: Vector3, n2: Vector3
    ): Triangle = apply( p0, n0, p1, n1, p2, n2, Material.Default )

  }

}
