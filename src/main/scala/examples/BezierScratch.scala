
package examples {

  import net.ruffy.marble.{ math, Env }
  import math.{ DoubleEnv }

  object BezierScratch extends Env with DoubleEnv {

    case class Vertex( point: Vector3, normal: Vector3 ) {
      def + ( v: Vertex ): Vertex = Vertex( point + v.point, normal + v.normal )
      def - ( v: Vertex ): Vertex = Vertex( point - v.point, normal - v.normal )

      def * ( x: Num ): Vertex = Vertex( point * x, normal * x )
      def / ( x: Num ): Vertex = Vertex( point / x, normal / x )
    }

    /*
       p00           p03
       * ------------*
       |             |
       |             |
       |             |
       * ------------*
       p30           p33
     */ 

    class BezierSurface(
      v00: Vertex, v01: Vertex, v02: Vertex, v03: Vertex,
      v10: Vertex, v11: Vertex, v12: Vertex, v13: Vertex,
      v20: Vertex, v21: Vertex, v22: Vertex, v23: Vertex,
      v30: Vertex, v31: Vertex, v32: Vertex, v33: Vertex
    ) {

      private val v = Array(
        Array( v00, v01, v02, v03 ),
        Array( v10, v11, v12, v13 ),
        Array( v20, v21, v22, v23 ),
        Array( v30, v31, v32, v33 )
      )

      private def getVertex( s: Num, t: Num ): Vertex = {
        import BezierSurface.{ getVertex => vt }

        val v0 = vt( v(0)(0), v(1)(0), v(2)(0), v(3)(0), s )
        val v1 = vt( v(0)(1), v(1)(1), v(2)(1), v(3)(1), s )
        val v2 = vt( v(0)(2), v(1)(2), v(2)(2), v(3)(2), s )
        val v3 = vt( v(0)(3), v(1)(3), v(2)(3), v(3)(3), s )

        vt( v0, v1, v2, v3, t )

      }

      def getVertex( i: Int, j: Int ): Vertex = v(i)(j)

      def setVertex( i: Int, j: Int, v: Vertex ): Unit = this.v(i)(j) = v

      def updateVertex( i: Int, j: Int, f: Vertex => Vertex ): Unit = v(i)(j) = f( v(i)(j) )

      def updatePoint( i: Int, j: Int, f: Vector3 => Vector3 ): Unit = updateVertex( i, j, {
        case Vertex( p, n ) => Vertex( f( p ), n )
      })

      def toPolygon( ns: Int, nt: Int ): Seq[Polygon] = {
        val ds = 1.0/ns
        val dt = 1.0/nt

        for( s <- 0 until ns; t <- 0 until nt ) yield {
          Rectangle(
            getVertex( s*ds, t*dt ).point,
            getVertex( (s+1)*ds, t*dt ).point,
            getVertex( (s+1)*ds, (t+1)*dt ).point,
            getVertex( s*ds, (t+1)*dt ).point
          )
        }

      }

    }

    object BezierSurface {

      def getVertex( v0: Vertex, v1: Vertex, v2: Vertex, v3: Vertex, t: Num ): Vertex = {
        val t3 = t * t * t
        val t2 = t * t
        v3 * t3  - v2 * 3 * t3  + v1 * 3 * t3  - v0 * t3  + v2 * 3 * t2  - v1 * 6 * t2  + v0 * 3 * t2  + v1 * 3 * t - v0 * 3 * t + v0
      }

      def apply( v0: Vertex, v1: Vertex, v2: Vertex, v3: Vertex ): BezierSurface = {

        val s = 1.0/3.0
        val t = 1 - s
        val a0 = v0 * t + v3 * s
        val a1 = v0 * s + v3 * t
        val a2 = v1 * t + v2 * s
        val a3 = v1 * s + v2 * t

        new BezierSurface(
          v0, v0 * t + v1 * s, v0 * s + v1 * t, v1,
          a0, a0 * t + a2 * s, a0 * s + a2 * t, a2,
          a1, a1 * t + a3 * s, a1 * s + a3 * t, a3,
          v3, v3 * t + v2 * s, v3 * s + v2 * t, v2
        )
      }

      def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3 ): BezierSurface = {
        val n = (p0 -> p1) x (p0 -> p2)
        apply( Vertex( p0, n ), Vertex( p1, n ), Vertex( p2, n ), Vertex( p3, n ) )
      }

    }

    def main( args: Array[String] ) {

      val img = new BufferedImage( 100, 100 )
      val screen = new Screen( img )
      val camera = DefaultCamera( screen, 20, 70.toRadians, 10.toRadians, 10 )
      val gc = Graphics( img )

      val sf = BezierSurface(
        Vector3(-1.0, 0.0, -1.0),  Vector3( 1.0, 0.0, -1.0),
        Vector3( 1.0, 0.0,  1.0),  Vector3(-1.0, 0.0,  1.0)
      )

      sf.updatePoint( 1, 1, v => Vector3( v.x, v.y + 1, v.z ) )
      sf.updatePoint( 1, 2, v => Vector3( v.x, v.y + 1, v.z ) )
      sf.updatePoint( 2, 1, v => Vector3( v.x, v.y + 1, v.z ) )
      sf.updatePoint( 2, 2, v => Vector3( v.x, v.y + 1, v.z ) )

      sf.toPolygon( 4, 4 ).foreach { p =>
        p.toCamera( camera ).drawWireFrame( gc, camera, Color.White.toInt )
      }

      img.write("png", "images/bezier.png")

    }

  }

}
