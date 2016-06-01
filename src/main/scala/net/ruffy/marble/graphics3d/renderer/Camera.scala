package net.ruffy.marble.graphics3d.renderer {

  import net.ruffy.marble.graphics2d.{ ColorEnv }
  import net.ruffy.util._
  import net.ruffy.marble.math.{ VectorEnv, MatrixEnv, MathEnv, NumberEnv }

  trait CameraEnv {

    env: MathEnv
        with NumberEnv
        with VectorEnv
        with MatrixEnv
        with MaterialEnv
        with ScreenEnv
        with PhotonEnv
        with ColorEnv =>

    import Math._
    import Material.{ Medium }

    /**
      * Camera
      * @param screen A screen to be projected by this camera.
      * @param position A position of this camera in world coordinates system.
      * @param nx A x axis of this camera coordinates system in world coordinates system.
      * @param ny A y axis of this camera coordinates system in world coordinates system.
      * @param nz A z axis of this camera coordinates system in world coordinates system.
      * @param focalLength A forcal length of this camera.
      */
    abstract class Camera {

      def screen: Screen
      def position: Vector3
      def nx: Vector3
      def ny: Vector3
      def nz: Vector3

      protected lazy val trans = Matrix4x4.transform( position, nx, nz )

      protected lazy val itrans = trans.inverse

      /** convert to the camera coordinate from the world coordinate. */
      def toCamera( p: Vector3 ): Vector3 = itrans * p

      /** convert to the screen coordinate from the camera coordinate. */
      def toScreen( p: Vector3 ): Vector3

      /** convert to the world coordinate from the camera coordinate. */
      def toWorld( p: Vector3 ): Vector3 = trans * p

      /** convert to the camera coordinate from the screen coordinate. */
      def position( x: Int, y: Int ): Vector3

      /** create a photon pointed to screen coordinate of (x, y). */
      def createPhoton( x: Int, y: Int ): Photon = {
        val s = position( x, y )
        // なぜoriginがsなのか？バグ？
        //Photon( s, Vector3.O -> s, Color.White, Medium.Default )
        Photon( Vector3.O, Vector3.O -> s, Color.White, Medium.Default )
      }

      /** create a photon pointed to screen coordinate of (x, y) on the world coordinate.*/
      def createPhotonOnWorld( x: Int, y: Int ): Photon = {
        val o = toWorld( Vector3.O )
        val s = toWorld( position( x, y ) )
        // なぜoriginがsなのか？バグ？
        //Photon( s, o -> s, Color.White, Medium.Default )
        Photon( o, o -> s, Color.White, Medium.Default )
      }
      
    }

    object Camera {

      /**
        * Z軸(カメラの向き, direction)とZ軸についての回転(rotation)からX, Y軸の方向のベクトルを計算する。
        * @param direction カメラの向き。Z軸の方向。
        * @param rotation カメラのZ軸についての回転。
        */
      def fromDirection( direction: Vector3, rotation: Num ): (Vector3, Vector3, Vector3) = {
        val nz = direction.normalize;
        val rot = Matrix4x4.rotation( direction, rotation )
        val nx = rot * (Vector3.Ny x nz)
        val ny = nz x nx
        (nx, ny, nz)
      }

      def fromPolar( r: Num, theta: Num, phi: Num ): Vector3 = Vector3( r*sin( theta )*cos( phi ), r*cos( theta ), r*sin( theta )*sin( phi ) )

    }

  }

}
