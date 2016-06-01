package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.marble.math._
  import net.ruffy.marble.graphics2d._
  import net.ruffy.marble.graphics3d.renderer._

  trait LightEnv {

    env: Shape3DEnv
        with BoundingSphereEnv
        with SphereEnv
        with RectangleEnv
        with PolygonsEnv
        with CameraEnv
        with MaterialEnv
        with IntersectionEnv
        with PhotonEnv
        with GraphicsEnv
        with ColorEnv
        with VectorEnv
        with MathEnv
        with NumberEnv =>

    abstract class Light extends Shape3D {
      type A = Light

      val shape: Shape3D

      lazy val boundingSphere = shape.boundingSphere

      lazy val center = shape.center

      lazy val material = shape.material

      def inside_?( p: Vector3 ) = shape.inside_?( p )

      def create( shape: Shape3D ): Light

      def getIntersection( ray: Photon ) = shape.getIntersection( ray )

      def setMaterial( material: Material ) = create(
        shape.setMaterial( material )
      )

      def transform( f: Vector3 => Vector3 ) = create(
        shape.transform( f )
      )

      def drawWireFrame(
        g: Graphics,
        camera: Camera,
        color: Int
      ) = shape.drawWireFrame( g, camera, color )

      def fill(
        g: Graphics,
        camera: Camera,
        lights: List[Light]
      ) = shape.fill( g, camera, lights )

      def fillWithGouraudShading(
        g: Graphics,
        camera: Camera,
        lights: List[Light],
        zbuf: ZBuffer
      ) = shape.fillWithGouraudShading( g, camera, lights, zbuf )

      def randomPosition = shape.randomPosition

      def createPhoton(
        color: Color
      ): Photon = shape.createPhoton( color )

      def color(
        t: Material.Type,
        i: Intersection
      ): Color = color( i.normal, i.point, i.shape.material, t )

      /**
        * レイトレース時に使用する。レイとの交差点での色を計算する。
        * @param n normal on the object
        * @param p point on the object
        * @param m material of the object
        * @param a type of material of the object
        */
      def color(
        n: Vector3,
        p: Vector3,
        m: Material,
        a: Material.Type
      ): Color

    }

    object Light {

      val attenuation: Num = 1.0
      val lambertFactor: Num = 0.0
      val photonPower: Num = 1.0

      /**
        * 点pに入射する光源lからの光量を計算する。
        */
//      def radiance(
//        l: Light,
//        n: Vector3,
//        p: Vector3,
//        m: Material,
//        a: Material.Type
//      ): Color = a match {
//
//        val v0 = p -> center
//        if( (v0 angle normal) > Math.Pi_2 ) {
//          //val (th, ph) = Math.angles( n, v0 )
//
//          val d = radius
//          val nt = normal * (v0 * normal)
//          val vr = v0 + (nt -> v0).normalize * d
//          val dt = vr angle v0
//
//          Photon.createRandomPhoton( dt, p, n, Color.Black, Medium.Default, None )
//
//          val t: Num = Math.sphereSurfaceIntegral( 1, 0, dt, 0, Math.Pix2 ) / Math.Pix2 * 20
//          //val t: Num = Math.sphereSurfaceIntegral( 1, th - dt, th + dt, ph - dt, ph + dt ) / Math.Pix2 * 10
//          println("### %s".format((nt -> v0, dt.toDegrees, t)))
//          //val t = dt/Math.Pi_2 * 5
//          m.surface.color(a) * t
//        } else Color.Black
//
//      }

      class Rectangle( val shape: Shape3D ) extends Light {

        private lazy val rect = shape.asInstanceOf[env.Rectangle]
        private lazy val normal: Vector3 = rect.normal.normalize
        private lazy val radius = (rect.center -> rect.p0).abs

        def create( shape: Shape3D ) = new Rectangle( shape )

        /**
          * 点pからこの光源を見た時の光源の外接円の視野角を計算する。
          * 視野角を２等分するベクトルとそのベクトルとのなす角を返す。
          */
        def viewAngle( p: Vector3, n: Vector3 ): Option[(Vector3, Num)] = {
          val v0 = p -> center
          //println( v0.abs, p, center, v0 angle n toDegrees )
          val ag = v0 angle normal
          // 点pがcenterの真下にある場合
          if( Math.equals(ag)(Math.Pi) ) {
            Some( (n, (p -> rect.p0) angle v0) )
          }
          // 点pがnormalに対する天頂角が90度以内にある場合
          else if( ag > Math.Pi_2 ) {
            val d = radius
            val nt = normal * (v0 * normal)
            val vs = nt -> v0
            val nr = vs.normalize * d // 光源の中心から円周までのベクトル
            val vr = (v0 + nr).normalize
            val vl = (v0 - nr).normalize
            //println( "*** ", ((vr + vl).normalize angle v0) toDegrees )
            Some( ((vr + vl).normalize, (vr angle vl)/2)  )
          }
          else None

        }

        /**
          * 点pを中心とする単位半球上に投影される光源の面積を統計的に求める。
          */
        def viewAreaStat( p: Vector3, n: Vector3, nphotons: Int ): Option[Num] = {
          viewAngle( p, n ) map { case (v0, a) =>
            var cnt1: Num = 0
            //println( a.toDegrees, n )
            for( i <- 1 to nphotons ) {
              val (th0, ph0) = Math.randomAngle( a )
              val dir = Math.createVector( v0, th0, ph0 )
              val th1 = n angle dir
              //val (th1, ph1) = Math.angles( n, Math.createVector( v0, th0, ph0 ) )
              //val photon = Photon( p, n, th1, ph1, Color.White, Material.Medium.Default, None )
              val photon = Photon( p, dir )
              //println( v0 angle photon.direction toDegrees )
              if( th1 < Math.Pi_2 ) rect.intersect( photon ) foreach { _ => cnt1 += 1 }
            }
            //println( cnt1, nphotons )
            val rate = cnt1 / nphotons
            Math.sphereSurfaceIntegral( 1, 0, a, 0, Math.Pix2 ) / Math.Pix2 * rate
          }
        }

        def colorMC( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
          case Material.Emittance => m.surface.emissionColor
          case _ => viewAreaStat( p, n, 100 ) match {
            case Some( x ) => m.surface.color(a) * x * 20
            case _ => Color.Black
          }
        }
      
        def colorViewAngle( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
          case Material.Emittance => m.surface.emissionColor
          case _ => 
            val v0 = p -> center
            if( (v0 angle normal) > Math.Pi_2 ) {
              //val (th, ph) = Math.angles( n, v0 )

              val d = radius
              val nt = normal * (v0 * normal)
              val vr = v0 + (nt -> v0).normalize * d
              val dt = vr angle v0

              //val t: Num = Math.sphereSurfaceIntegral( 1, 0, dt, 0, Math.Pix2 ) / Math.Pix2 * 20
              //val t: Num = Math.sphereSurfaceIntegral( 1, th - dt, th + dt, ph - dt, ph + dt ) / Math.Pix2 * 10
              //println("### %s".format((nt -> v0, dt.toDegrees, t)))
              val t = dt/Math.Pi_2 * 5
              m.surface.color(a) * t
            } else Color.Black

            //val lp = (center -> p).normalize
            //val s = (normal * lp) * ( (-lp) * n )
            //val t: Num = if( s < 0 ) 0f else s
            //m.surface.color(a) * t
        }

        def color( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = colorViewAngle( n, p, m, a )
      }

      object Rectangle {

        def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3 ): Rectangle =
          new Rectangle( env.Rectangle( p0, p1, p2, p3, Material.WhiteLight ) )

      }

      class Sphere ( val shape: Shape3D ) extends Light {

        def create( shape: Shape3D ) = new Sphere( shape )

        def color( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
          case Material.Emittance => m.surface.emissionColor
          case _ =>
            val s = n * (p -> center).normalize
            val t: Num = if( s < 0 ) 0f else s
            m.surface.color(a) * t
        }
      }

      object Sphere {

        def apply( center: Vector3, radius: Num ): Sphere = new Sphere( BoundingSphere( center, radius, Material.WhiteLight ) )

      }

      def color( t: Material.Type, i: Intersection ): Color = i.shape.material.surface.color( t )

      //    def getColor( light: Shape3D )( t: Material.Type, i: Intersection ): Color = getColor( light, i.normal, i.point, i.material, t )
      //
      //    def getColor( light: Shape3D, n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
      //      case Material.Emittance => m.color
      //      case _ =>
      //        val lv = (p -> light.center).normalize
      //        //val lv = -Vector3.Ny
      //        //val nn = (light.center -> p).normalize
      //        val s = n * lv
      //        //println( ">>> "+n+", "+lv+", "+s )
      //        //println( ">>> "+(n angle lv)+", "+s )
      //        val t: Num = if( s < 0 ) 0f else s
      //        //println( "+++ "+(m.color *t))
      //        m.color * t
      //    }

    }

  }
}
