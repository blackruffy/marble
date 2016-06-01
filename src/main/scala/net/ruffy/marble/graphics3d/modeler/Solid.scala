package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.marble._
  import graphics2d._
  import graphics3d.renderer._
  import net.ruffy.marble.math._
  import net.ruffy.util.{ Implicits, Result }
  import Implicits._

  trait SolidEnv {

    env: Shape3DEnv
        with MaterialEnv
        with LightEnv
        with ColorEnv
        with PolygonEnv
        with PolygonsEnv
        with IntersectionEnv
        with PhotonEnv
        with CameraEnv
        with GraphicsEnv
        with VectorEnv
        with NumberEnv
        with MathEnv =>

    import Intersection.near

    /**
      * 多角形(ポリゴン)で構成される３次元の物体。
      * Shape3Dのほとんどのメソッドを実装している。
      * @param polygons オブジェクトを構成するポリゴン
      */
    abstract class Solid( _polygons: List[Polygon], val material: Material ) extends Shape3D {

      //type A = Solid

      lazy val polygons = _polygons.map( _.setMaterial( material ) )

      private lazy val randomPolygon = new Polygons.RandomPolygon( polygons )

      lazy val numberOfPolygons = polygons.size

      lazy val center = Shape3D.getCenter( polygons )

      lazy val boundingSphere = Shape3D.getBoundingSphere( polygons, center )

      def inside_?( p: Vector3 ): Boolean = polygons.forall( _.inside_?( p ) )

      //def create( polygons: List[Polygon] ) = new Solid( polygons, material )

      def create( polygons: List[Polygon] ): A = create( polygons, material )

      def create( polygons: List[Polygon], material: Material ): A

      def transform( f: Vector3 => Vector3 ) = create( polygons.map( _.transform( f ) ) )

      def setMaterial( material: Material ) = create( polygons, material )
      
      def getIntersection( ray: Photon ): Result[Intersection, Intersection] = Polygons.getIntersection( polygons, ray )

      def drawWireFrame( g: Graphics, camera: Camera, color: Int ): Unit = polygons.foreach( _.drawWireFrame( g, camera, color ) )

      def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = polygons.foreach( _.fill( g, camera, lights ) )

      def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ): Unit = polygons.foreach( _.fillWithGouraudShading( g, camera, lights, zbuf ) )

      def randomPosition = randomPolygon.pickup.randomPosition

      def createPhoton( color: Color ) = randomPolygon.pickup.createPhoton( color )

    }

    object Solid {

      class Impl( polygons: List[Polygon], material: Material ) extends Solid( polygons, material ) {
        type A = Solid
        def create( polygons: List[Polygon], material: Material ): Solid = new Impl( polygons, material )
      }

    }

  }
}
