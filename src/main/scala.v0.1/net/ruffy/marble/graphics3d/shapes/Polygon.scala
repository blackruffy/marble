package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math._
  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d._
  import net.ruffy.marble.graphics3d._

  /**
    * 多角形を表現する。
    */
  abstract class Polygon extends Shape3D {

    type A = Polygon

    def points: List[Vector3]
    def normals: List[Vector3]
    def material: Material

    def normal: Vector3

    def inside_?( p: Vector3 ): Boolean

    def create( points: List[Vector3], normals: List[Vector3], material: Material ): A

    def map( f: (Vector3, Vector3) => (Vector3, Vector3) ):  (List[Vector3], List[Vector3]) = Polygon.map( f, points, normals )

    def transform( f: Vector3 => Vector3 ) = {
      val (ps, ns) = map { (p, n) =>
        val q = f( p )
        val m = q -> f( p + n )
        (q, m)
      }
      create( ps, ns, material )
    }

    def setMaterial( material: Material ) = create( points, normals, material )

    def drawWireFrame( g: Graphics, camera: Camera, color: Int ): Unit = {
      val screen = camera.screen
      val pixs = points.map( camera.toScreen _ >> screen.pixel _ )
      val (x0, y0, z0) = pixs.head
      def drawLine( pixs: List[(Int, Int, Int)] ): Unit = pixs |> {
        case (x, y, z) :: Nil => g.drawLine( x, y, x0, y0, color )
        case (x1, y1, z1) :: (p2@(x2, y2, z2)) :: ps =>
          g.drawLine( x1, y1, x2, y2, color )
          drawLine( p2 :: ps )
      }
      drawLine( pixs )
    }

    override lazy val toString: String = points.mkString("Polygon(", ", ", ")")

  }


  object Polygon {

    def map( f: (Vector3, Vector3) => (Vector3, Vector3), ps: List[Vector3], ns: List[Vector3] ): (List[Vector3], List[Vector3]) = ( ps, ns ) match {
      case ( Nil, Nil ) => ( Nil, Nil )
      case ( x :: xs, y :: ys ) =>
        val (a, b) = f( x, y )
        val (as, bs) = map( f, xs, ys )
        ( a :: as, b :: bs )
    }

  }

}
