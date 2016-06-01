package net.ruffy.marble.graphics3d.renderer {

  import net.ruffy.marble.graphics2d.{ ColorEnv }
  import net.ruffy.util._
  import net.ruffy.marble.math.{ VectorEnv, MatrixEnv, MathEnv, NumberEnv }

  trait DefaultCameraEnv {

    env: CameraEnv
        with ScreenEnv
        with VectorEnv
        with MatrixEnv
        with MathEnv
        with NumberEnv
        with MaterialEnv
        with ColorEnv =>

    import Math._
    import Material.{ Medium }

    /**
      * DefaultCamera
      * @param screen A screen to be projected by this camera.
      * @param position A position of this camera in world coordinates system.
      * @param nx A x axis of this camera coordinates system in world coordinates system.
      * @param ny A y axis of this camera coordinates system in world coordinates system.
      * @param nz A z axis of this camera coordinates system in world coordinates system.
      * @param focalLength A forcal length of this camera.
      */
    class DefaultCamera(
      val screen: Screen,
      val position: Vector3,
      val nx: Vector3,
      val ny: Vector3,
      val nz: Vector3,
      val focalLength: Num
    ) extends Camera {

      /** convert to the screen coordinate from the camera coordinate. */
      def toScreen( p: Vector3 ) = DefaultCamera.toScreen( p, focalLength )

      /** convert to the camera coordinate from the screen coordinate. */
      def position( x: Int, y: Int ) = Vector3( screen.screenX( x ), screen.screenY( y ), focalLength )
      
    }

    object DefaultCamera {

      /**
        * Create a camera instance with a z-direction and a rotation of camera.
        * @param screen A screen to be projected by this camera.
        * @param position A position of this camera in the current coordinates system.
        * @param rotation A rotation angle in radians of the direction from the x axis to the y axis about the 'direction' axis.
        * @param focalLength A focal length of this camera.
        */
      def apply( screen: Screen, position: Vector3, direction: Vector3, rotation: Num, focalLength: Num ): DefaultCamera = {
        val (nx, ny, nz) = Camera.fromDirection( direction, rotation )
        new DefaultCamera( screen, position, nx, ny, nz, focalLength )
      }

      def apply( screen: Screen, r: Num, theta: Num, phi: Num, focalLength: Num ): DefaultCamera = apply( screen, r, theta, phi, 0f, focalLength )

      def apply( screen: Screen, r: Num, theta: Num, phi: Num, rotation: Num, focalLength: Num ): DefaultCamera = {
        val p = Camera.fromPolar( r, theta, phi )
        apply( screen, p, p -> Vector3.O, rotation, focalLength )
      }

      def toScreen( p: Vector3, focalLength: Num ) = Vector3(
        p.x*focalLength/p.z,
        p.y*focalLength/p.z,
        p.z
      )

    }

  }
}
