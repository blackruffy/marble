package net.ruffy.marble.graphics3d.renderer {

  import net.ruffy.marble.math.{ MathEnv, VectorEnv, NumberEnv }
  import net.ruffy.marble.graphics3d.modeler.{ Shape3DEnv }
  import net.ruffy.util.{ Result, Good, Bad, Empty }

  trait IntersectionEnv {

    env: MathEnv
        with NumberEnv
        with VectorEnv
        with Shape3DEnv
        with MaterialEnv
        with PhotonEnv =>

    import Material.{ Medium }

    /**
      * 衝突点の情報を保持するクラス。
      * @param ray 入射光
      * @param distance 発射点からの距離
      * @param material 衝突点の材質
      * @param point 衝突点
      * @param normal 衝突点の法線
      */
    case class Intersection( ray: Photon, distance: Num, shape: Shape3D, point: Vector3, normal: Vector3 ) {

      lazy val inside_? = (ray.direction angle normal) < Math.Pi_2

      /**
        * 反射光を作成する。
        */
      lazy val createReflection: Photon = Photon(
        point,
        ray.direction.reflect( if( inside_? ) -normal else normal ),
        Photon.composeColor( ray.color, shape.material.surface.reflectiveColor ),
        ray.medium,
        Some(ray)
      )

      /**
        * 屈折光を作成する。
        * @param m 射出側の媒質
        * @return 屈折光。
        */
      def createRefraction( m: Medium ): Photon = {
        val n = if( inside_? ) -normal else normal
        val u = ray.direction.refract( n, ray.medium.refractiveIndex, m.refractiveIndex )
        if( ( n angle u) > Math.Pi_2 ) Photon( point, u, Photon.composeColor( ray.color, shape.material.surface.refractiveColor ), m, Some( ray ) )
        else createReflection
      }

      def createPhoton( direction: Vector3 ): Photon = Photon( point, direction, shape.material.surface.diffuseColor, ray.medium, Some(ray) )

      def createPhoton( theta: Num, phi: Num ): Photon = Photon.createPhoton( point, normal, theta, phi, shape.material.surface.diffuseColor, ray.medium, Some(ray) )

      /**
        * 拡散面のフォトンをランダムに生成する。
        */
      def createPhotonOnDiffuse: Photon = Photon.createRandomPhoton(
        point,
        if( inside_? ) -normal else normal,
        Photon.composeColor( ray.color, shape.material.surface.diffuseColor ),
        ray.medium,
        Some( ray )
      )

      /**
        * 拡散面のフォトンをランダムに生成する。
        */
      def createPhotonOnDiffuse( dth: Num ): Photon = Photon.createRandomPhoton(
        dth,
        point,
        if( inside_? ) -normal else normal,
        Photon.composeColor( ray.color, shape.material.surface.diffuseColor ),
        ray.medium,
        Some( ray )
      )

    }

    object Intersection {

      def near( i0: Result[Intersection, Intersection] )( i1: Result[Intersection, Intersection] ): Result[Intersection, Intersection] = i0.flatMap { j0 =>
        i1.map { j1 => if( j0.distance < j1.distance ) j0 else j1 } orElse i0
      } orElse i1

    }

  }
}
