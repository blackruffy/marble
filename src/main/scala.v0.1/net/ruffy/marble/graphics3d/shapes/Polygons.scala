package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math.{ Vector3 }
  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ Color, Graphics, ZBuffer }
  import net.ruffy.marble.graphics3d.{ Material, Camera, Photon, Intersection }
  import Material.{ Surface }

  /**
    * 三角形で構成される多角形を表現する。
    */
  abstract class Polygons( val points: List[Vector3], val normals: List[Vector3], val material: Material, toTriangles: (List[Vector3], List[Vector3], Material) => List[Triangle] ) extends Polygon {

    lazy val triangles = toTriangles( points, normals, material )
    lazy val numberOfTriangles = triangles.size
    lazy val normal = triangles.head.normal
    lazy val center = Shape3D.getCenter( triangles )
    lazy val boundingSphere = Shape3D.getBoundingSphere( triangles, center )
    private lazy val randomPolygon = new Polygons.RandomPolygon( triangles )

    /**
      * 点pが内側にあるか判定する。
      */
    def inside_?( p: Vector3 ) = triangles.forall( _.inside_?( p ) )

    def getIntersection( ray: Photon ) = Polygons.getIntersection( triangles, ray )

    def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = {
      triangles.foreach( _.fill( g, camera, lights ) )
    }

    def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ) = triangles.foreach( _.fillWithGouraudShading( g, camera, lights, zbuf ) )

    def randomPosition = randomPolygon.pickup.randomPosition

    def createPhoton( color: Color ) = randomPolygon.pickup.createPhoton( color )
  }

  object Polygons {

    def toTriangles( points: List[Vector3], normals: List[Vector3], material: Material ): List[Triangle] = {
      val p0 = points.head
      val n0 = normals.head
      def func( ps: List[Vector3], ns: List[Vector3] ): List[Triangle] = (ps, ns) |> {
        case ( Nil, Nil ) => Nil
        case (p1 :: p2 :: Nil, n1 :: n2 :: Nil ) => (new Triangle( p0, n0, p1, n1, p2, n2, material )) :: Nil
        case (p1 :: p2 :: xs, n1 :: n2 :: ys) => (new Triangle( p0, n0, p1, n1, p2, n2, material )) :: func( p2 :: xs, n2 :: ys )
      }
      func( points.tail, normals.tail )
    }

    def getIntersection( polygons: List[Polygon], ray: Photon ): Option[Intersection] = polygons match {
      case Nil => None
      case x :: xs => x.getIntersection( ray ) |> Intersection.near( getIntersection( xs, ray ) )
    }

    class RandomPolygon( polygons: List[Polygon] ) {

      private var rest: List[Polygon] = Nil

      def pickup: Polygon = rest match {
        case Nil =>
          rest = polygons.tail
          polygons.head
        case x :: xs =>
          rest = xs
          x
      }

    }

  }

}
