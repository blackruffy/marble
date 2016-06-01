package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math.{ Matrix4x4, Vector3, Math, WrappedFloat }
  import net.ruffy.util._
  import net.ruffy.marble.graphics2d.{ Graphics, ZBuffer, Color }
  import net.ruffy.marble.graphics3d.{ Camera, Photon, Material, Intersection }
  import Material.{ Medium }

  import Implicits._
  import WrappedFloat._

  class BoundingSphere( val center: Vector3, val radius: Float, val material: Material ) extends Shape3D {

    type A = BoundingSphere

    private lazy val pr = center + Vector3( 0f, 0f, radius )

    lazy val sphere = Sphere( radius, material ).transform( Matrix4x4.translate( center.x, center.y, center.z ) )

    lazy val boundingSphere = this

    def normal( p: Vector3 ): Vector3 = (center -> p).normalize

    def setMaterial( material: Material ) = new BoundingSphere( center, radius, material )

    def getIntersection( ray: Photon ): Option[Intersection] = {
      val o = ray.origin
      val d = ray.direction
      val d2 = d.abs2
      val oc = o - center
      val oc2 = oc.abs2
      val r2 = radius * radius

      val a = d2
      val b = d * oc
      val c = oc2 - r2
      val e = b*b - a * c
      val aprx = (x: Float) => if( Math.equals(x)(0) ) 0 else x
      
      if( e < 0 ) None else {
        val e_2 = Math.sqrt(e)
        val t1 = (-b + e_2)/a |> aprx
        val t2 = (-b - e_2)/a |> aprx
        if( t1 <= 0 && t2 <= 0 ) None else {
          val t = if( t1 > 0 && t2 <= 0 ) t1 else if( t1 <= 0 && t2 > 0 ) t2 else if(t1 > 0 && t2 > 0) t1 min t2 else throw new Exception("unexpected solution of intersection")
          val p = o + d * t
          Some( Intersection(ray, t, this, p, normal( p ) ) )
        }
      }
    }

    def inside_?( p: Vector3 ): Boolean = (center -> p).abs < radius

    def transform( f: Vector3 => Vector3 ) = {
      val c = f( center )
      val p = f( pr )
      val rd = (c -> p).abs
      new BoundingSphere( c, rd, material )
    }

    def drawWireFrame( g: Graphics, camera: Camera, color: Int ): Unit = sphere.drawWireFrame( g, camera, color )

    def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = sphere.fill( g, camera, lights )

    def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ): Unit = sphere.fillWithGouraudShading( g, camera, lights, zbuf )

    def randomPosition: Vector3 = {
      val theta = Math.random * Math.Pi
      val phi = Math.random * Math.Pix2
      Vector3( Math.cos(phi) * Math.cos(theta), Math.cos(theta), Math.sin(phi) * Math.cos(theta) ) + center
    }

    def createPhoton( color: Color ): Photon = {
      val theta = Math.random * Math.Pi
      val phi = Math.random * Math.Pix2
      Photon(
        center,
        Vector3( Math.cos(phi) * Math.cos(theta), Math.cos(theta), Math.sin(phi) * Math.cos(theta) ),
        color,
        Medium.Default
      )
    }

    override def toString = "center: %s, radius: %f".format( center, radius )
  }

  object BoundingSphere {

    def apply( center: Vector3, radius: Float ): BoundingSphere = new BoundingSphere( center, radius, Material.Default )

    def apply( radius: Float ): BoundingSphere = new BoundingSphere( Vector3.O, radius, Material.Default )

    def apply( center: Vector3, radius: Float, material: Material ): BoundingSphere = new BoundingSphere( center, radius, material )

    def apply( radius: Float, material: Material ): BoundingSphere = new BoundingSphere( Vector3.O, radius, material )

    /**
      * 1が2を含んでいる: 1
      * 2が1を含んでいる: 2
      * ２つのオブジェクトは、離れている: 3
      * ２つのオブジェクトは、接している: 4
      */
    def contains( c1: Vector3, r1: Float, c2: Vector3, r2: Float ): Int =  {
      val d = (c1 -> c2).abs
      if( r1 > d + r2 ) 1
      else if( r2 > d + r1 ) 2
      else if( d > r1 + r2 ) 3
      else 4
    }

    def contains( b1: BoundingSphere, b2: BoundingSphere ): Int = contains( b1.center, b1.radius, b2.center, b2.radius )

  }

}
