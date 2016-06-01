package net.ruffy.marble.graphics3d.renderer {

  import net.ruffy.marble.graphics2d.{ ColorEnv }
  import net.ruffy.marble.math.{ MathEnv, VectorEnv, MatrixEnv, NumberEnv, CoordinateSystemEnv }
  import net.ruffy.util._
  import Implicits._

  trait PhotonEnv {

    env: ColorEnv
        with VectorEnv
        with MathEnv
        with NumberEnv
        with MaterialEnv
        with MatrixEnv
        with CoordinateSystemEnv =>

    import Material._

    /**
      * フォトンを表現するクラス。
      * @param medium このフォトンが存在する媒質
      * @param materials フォトンが通ってきた材質のスタック。屈折率を使用するため。
      */
    class Photon( val origin: Vector3, val direction: Vector3, val color: Color, val medium: Medium, val parent: Option[Photon] ) {

      override def toString = "Photon( origin: %s, direction: %s )".format( origin, direction )

    }

    object Photon {

      //def composeColor( c1: Color, c2: Color ): Color = c1 * c2
      def composeColor( c1: Color, c2: Color ): Color = c2

      def apply( origin: Vector3, direction: Vector3 ): Photon = new Photon( origin, direction, Color.White, Medium.Default, None )

      def apply( origin: Vector3, direction: Vector3, color: Color, medium: Medium, parent: Option[Photon] ): Photon = new Photon( origin, direction, color, medium, parent )

      def apply( origin: Vector3, direction: Vector3, color: Color, medium: Medium ): Photon = new Photon( origin, direction.normalize, color, medium, None )

      /**
        * ベクトルnに対する天頂角thetaと方位角phiで決定される方向へのフォトンを作成する。
        */
      def apply( p: Vector3, n: Vector3, theta: Num, phi: Num, color: Color, medium: Medium, parent: Option[Photon] ): Photon = {
        createPhoton( p, n, theta, phi, color, medium, parent )
      }

      /**
        * ベクトルnに対する天頂角thetaと方位角phiで決定される方向へのフォトンを作成する。
        */
      def createPhoton( p: Vector3, n: Vector3, theta: Num, phi: Num, color: Color, medium: Medium, parent: Option[Photon] ): Photon = {
        Photon( p, Math.createVector( n, theta, phi ), color, medium, parent )
      }

      /**
        * ベクトルnに対する天頂角dthで指定した範囲内でランダムにフォトンを生成する。
        * 方位角は0から360度まで。
        * @param dth 0度から90度まで(ラジアンで)
        */
      def createRandomPhoton(
        dth: Num,
        p: Vector3,
        n: Vector3,
        color: Color,
        medium: Medium,
        parent: Option[Photon]
      ): Photon = {
        val (theta1, phi1) = Math.randomAngle( dth )
        createPhoton( p, n, theta1, phi1, color, medium, parent )
      }

      /**
        * 点pから法線nが作る半球上のランダムに選んだ点に向うフォトンを作成する。
        * @param color 作成されるフォトンが保持する色。
        * @param medium 作成されるフォトンが向う媒質。
        */
      def createRandomPhoton( p: Vector3, n: Vector3, color: Color, medium: Medium, parent: Option[Photon] ): Photon = {
        val theta = Math.acos( Math.sqrt( Math.random ) )
        val phi = Math.random * Math.Pix2
        apply( p, n, theta, phi, color, medium, parent )
      }

    }

  }

}
