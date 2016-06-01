package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.marble.math.{ MatrixEnv, VectorEnv, MathEnv, NumberEnv }
  import net.ruffy.util.{ Implicits, Result, Empty, Good, Bad }
  import net.ruffy.marble.graphics2d.{ GraphicsEnv, ZBuffer, ColorEnv }
  import net.ruffy.marble.graphics3d.renderer.{ CameraEnv, PhotonEnv, MaterialEnv, IntersectionEnv }

  import Implicits._

  trait BoundingSphereEnv {

    env: Shape3DEnv
        with SolidEnv
        with LightEnv
        with GraphicsEnv
        with ColorEnv
        with CameraEnv
        with PhotonEnv
        with MaterialEnv
        with IntersectionEnv
        with VectorEnv
        with MatrixEnv
        with SphereEnv
        with MathEnv
        with NumberEnv =>

    import Material.{ Medium }

    class BoundingSphere( val center: Vector3, val radius: Num, val material: Material ) extends Shape3D {

      type A = BoundingSphere

      private lazy val pr = center + Vector3( 0f, 0f, radius )

      lazy val sphere = Sphere( radius, material ).transform( Matrix4x4.translate( center.x, center.y, center.z ) )

      lazy val boundingSphere = this

      def normal( p: Vector3 ): Vector3 = (center -> p).normalize

      def setMaterial( material: Material ) = new BoundingSphere( center, radius, material )

      def getIntersection( ray: Photon ): Result[Intersection, Intersection] = {
        val o: Vector3 = ray.origin
        val d: Vector3 = ray.direction
        val d2: Num = d.abs2
        val oc: Vector3 = o - center
        val oc2: Num = oc.abs2
        val r2: Num = radius * radius

        val a: Num = d2
        val b: Num = d * oc
        val c: Num = oc2 - r2
        val e: Num = b*b - a * c
        val aprx: Num => Num = x => if( Math.equals(x)(0) ) 0 else x

        if( e < 0 ) Empty // 虚数解
        else {
          val e_2: Num = Math.sqrt(e)
          val t1: Num = (-b + e_2)/a |> aprx // ２時方程式の解
          val t2: Num = (-b - e_2)/a |> aprx // ２時方程式の解
          if( t1 <= 0 && t2 <= 0 ) Empty else {
            val t =
              if( t1 > 0 && t2 <= 0 ) t1
              else if( t1 <= 0 && t2 > 0 ) t2
              else if(t1 > 0 && t2 > 0) t1 min t2
              else throw new Exception("unexpected solution of intersection: %s, %s %s".format(t1, t2, List(o, d, d2, oc, oc2, r2, a, b, c, e).mkString("\n", "\n", "")))
            val p = o + d * t
            Good( Intersection(ray, t, this, p, normal( p ) ) )
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

      def apply( center: Vector3, radius: Num ): BoundingSphere = new BoundingSphere( center, radius, Material.Default )

      def apply( radius: Num ): BoundingSphere = new BoundingSphere( Vector3.O, radius, Material.Default )

      def apply( center: Vector3, radius: Num, material: Material ): BoundingSphere = new BoundingSphere( center, radius, material )

      def apply( radius: Num, material: Material ): BoundingSphere = new BoundingSphere( Vector3.O, radius, material )

      /**
        * 1が2を含んでいる: 1
        * 2が1を含んでいる: 2
        * ２つのオブジェクトは、離れている: 3
        * ２つのオブジェクトは、接している: 4
        */
      def contains( c1: Vector3, r1: Num, c2: Vector3, r2: Num ): Int =  {
        val d = (c1 -> c2).abs
        if( r1 > d + r2 ) 1
        else if( r2 > d + r1 ) 2
        else if( d > r1 + r2 ) 3
        else 4
      }

      def contains( b1: BoundingSphere, b2: BoundingSphere ): Int = contains( b1.center, b1.radius, b2.center, b2.radius )

    }

  }
}
