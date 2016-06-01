package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.util.{ Implicits, Result }
  import net.ruffy.marble.math.{ MathEnv, VectorEnv, MatrixEnv, NumberEnv }
  import net.ruffy.marble.graphics3d.renderer.{ IntersectionEnv, CameraEnv, PhotonEnv, MaterialEnv }
  import net.ruffy.marble.graphics2d.{ GraphicsEnv, ZBuffer, ColorEnv }
  import Implicits._

  trait Shape3DEnv {

    env: BoundingSphereEnv
        with PhotonEnv
        with CameraEnv
        with MaterialEnv
        with IntersectionEnv
        with LightEnv
        with GraphicsEnv
        with ColorEnv
        with MathEnv
        with NumberEnv
        with NumberEnv
        with VectorEnv
        with PolygonEnv
        with MatrixEnv =>

    /**
      * ３次元の物体を表現する抽象クラス。
      * 本クラスを実装したサブクラスは、全て描画可能となる。
      */
    abstract class Shape3D {

      type A <: Shape3D

      def center: Vector3

      def boundingSphere: BoundingSphere

      def material: Material

      def intersect( ray: Photon ): Result[Intersection, Intersection] = {
        boundingSphere.getIntersection( ray ).flatMap( _ => getIntersection(ray) )
      }

      def inside_?( p: Vector3 ): Boolean

      /**
        * sが含まれている: 1
        * sが含んでいる: 2
        * ２つのオブジェクトは、離れている: 3
        * ２つのオブジェクトは、接している: 4
        */
      def contains( s: Shape3D ): Int = BoundingSphere.contains( boundingSphere, s.boundingSphere )

      /** 包含関係(sが含んでいる) */
      def < ( s: Shape3D ): Boolean = contains( s ) == 2

      /** 包含関係(sが含まれている) */
      def > ( s: Shape3D ): Boolean = contains( s ) == 1

      /** ２つのオブジェクトは、離れている(矢印が互いに反対方向) */
      def <> ( s: Shape3D ): Boolean = contains( s ) == 3

      /** ２つのオブジェクトは、接している(矢印が互いに向き合っている) */
      def >< ( s: Shape3D ): Boolean = contains( s ) == 4

      def getIntersection( ray: Photon ): Result[Intersection, Intersection]

      def setMaterial( material: Material ): A

      def transform( f: Vector3 => Vector3 ): A

      def toCamera( camera: Camera ): A = transform( camera.toCamera _ )

      def transform( matrix: Matrix4x4 ): A = transform( p => matrix * p )

      def drawWireFrame( g: Graphics, camera: Camera, color: Int ): Unit

      def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit

      def fill( g: Graphics, camera: Camera, light: Light ): Unit = fill( g, camera, light :: Nil )

      def fillWithGouraudShading( g: Graphics, camera: Camera, light: Light, zbuf: ZBuffer ): Unit = fillWithGouraudShading( g, camera, light :: Nil, zbuf )

      def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ): Unit

      def randomPosition: Vector3

      /**
        * 光源用
        */
      def createPhoton( color: Color ): Photon

    }

    object Shape3D {

      type AccumCenterType = (Vector3, Int)
      type AccumCenterFunc = (AccumCenterType, Shape3D) => AccumCenterType
      type AccumSphereType = Num
      type AccumSphereFunc = (AccumSphereType, Shape3D) => AccumSphereType

      lazy val accumCenter: AccumCenterFunc = { case ((a, n), b) => ( a + b.center, n + 1 ) }

      def getCenter[A](
        shapes: A,
        foldLeft: (A, AccumCenterType, AccumCenterFunc) => AccumCenterType
      ): Vector3 = {
        val (a, n) = foldLeft( shapes, (Vector3.O, 0), accumCenter )
        a / n
      }

      def getCenter( shapes: List[Shape3D] ): Vector3 = getCenter[List[Shape3D]]( shapes, (xs, a, f) => xs.foldLeft( a )( f ) )

      def getBoundingSphere[A](
        shapes: A,
        center: Vector3,
        foldLeft: (A, AccumSphereType, AccumSphereFunc) => AccumSphereType
      ): BoundingSphere = {

        val r = foldLeft( shapes, Math._0, {

          case (max, s: Polygon) =>
            val r = s.points.foldLeft( Math._0 ) { (m , p) =>
              val r = (center -> p).abs
              if( r > m ) r else m
            }
            if( r > max ) r else max

          case (max, s) => 
            s.boundingSphere |> { bs =>
              val r = (bs.center -> center).abs +  bs.radius
              if( r > max ) r else max
            }

        } )

        BoundingSphere( center, r )
      }

      def getBoundingSphere(
        shapes: List[Shape3D],
        center: Vector3
      ): BoundingSphere = getBoundingSphere[List[Shape3D]]( shapes, center, (xs, a, f) => xs.foldLeft( a )( f ) )

    }

  }
}
