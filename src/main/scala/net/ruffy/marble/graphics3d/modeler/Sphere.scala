package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.marble._
  import graphics2d._
  import graphics3d.renderer._
  import math.{ MathEnv, VectorEnv, NumberEnv }

  trait SphereEnv {

    env: MathEnv
        with NumberEnv
        with Shape3DEnv
        with SolidEnv
        with TriangleEnv
        with RectangleEnv
        with VectorEnv
        with MaterialEnv =>

    import Math._

    class Sphere( radius: Num, ndiv: Int, material: Material ) extends Solid.Impl(
      Sphere.createPolygons( radius, ndiv, material ),
      material
    )

    object Sphere {

      def createPolygons( radius: Num, ndiv: Int, material: Material ) = {
        val istep: Num = 180/ndiv
        val jstep: Num = 360/ndiv
        val normals = new Array[Array[Vector3]](ndiv+1)

        for {
          i <- (0 until ndiv).toList
          j <- (0 until ndiv).toList
        } yield {

          val the0: Num = (env.mkNum(i-1)*istep).toRadians
          val the1: Num = (env.mkNum(i)*istep).toRadians
          val the2: Num = (env.mkNum(i+1)*istep).toRadians
          val the3: Num = (env.mkNum(i+2)*istep).toRadians

          val phi0: Num = (env.mkNum(j-1)*jstep).toRadians
          val phi1: Num = (env.mkNum(j)*jstep).toRadians
          val phi2: Num = (env.mkNum(j+1)*jstep).toRadians
          val phi3: Num = (env.mkNum(j+2)*jstep).toRadians

          def mkPoint( t: Num, p: Num ) = Vector3( radius*sin(t)*sin(p), radius*cos(t), radius*sin(t)*cos(p) )

          def ps = Array(
            Array(
              Vector3.O,
              mkPoint( the0, phi1 ),
              mkPoint( the0, phi2 ),
              Vector3.O
            ),
            Array(
              mkPoint( the1, phi0 ),
              mkPoint( the1, phi1 ),
              mkPoint( the1, phi2 ),
              mkPoint( the1, phi3 )
            ),
            Array(
              mkPoint( the2, phi0 ),
              mkPoint( the2, phi1 ),
              mkPoint( the2, phi2 ),
              mkPoint( the2, phi3 )
            ),
            Array(
              Vector3.O,
              mkPoint( the3, phi1 ),
              mkPoint( the3, phi2 ),
              Vector3.O
            )
          )

          def mkNormal( p: Int, q: Int ) = {
            if( i == 0 ) Vector3.Ny else {
              (Triangle.normal(ps(p)(q), ps(p-1)(q), ps(p)(q-1))
                + Triangle.normal(ps(p)(q), ps(p)(q+1), ps(p-1)(q))
                + Triangle.normal(ps(p)(q), ps(p+1)(q), ps(p)(q+1))
                + Triangle.normal(ps(p)(q), ps(p)(q-1), ps(p+1)(q))
              ).normalize
            }
          }

          def getNormal( p: Int, q: Int ) = {
            if( normals(p) == null ) normals(p) = new Array[Vector3](ndiv + 1)
            normals(p)(q) match {
              case null =>
                val n = mkNormal( p-i+1, q-j+1 )
                normals(p)(q) = n
                n
              case n => n
            }
          }

          Rectangle(
            ps(1)(1), getNormal( i, j ),
            ps(1)(2), getNormal( i, j+1 ),
            ps(2)(2), getNormal( i+1, j+1 ),
            ps(2)(1), getNormal( i+1, j ),
            material
          )

        }

      }

      def apply( radius: Num, material: Material ): Sphere = new Sphere( radius, 10, material )

      def apply( radius: Num ): Sphere = new Sphere( radius, 10, Material.Default )

    }

  }
}
