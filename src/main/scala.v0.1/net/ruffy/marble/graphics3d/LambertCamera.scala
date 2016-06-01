package net.ruffy.marble.graphics3d {


  import net.ruffy.marble.graphics2d.{ Color }
  import net.ruffy.util._
  import net.ruffy.marble.math.{ Vector3, Matrix4x4, Math }
  import Math._
  import Material.{ Medium }

  class LambertCamera(
    val screen: Screen,
    val position: Vector3,
    val nx: Vector3,
    val ny: Vector3,
    val nz: Vector3
  ) extends Camera {

    /** スクリーンの幅 */
    private lazy val s = screen.ymax - screen.ymin

    /** スクリーンの半径 */
    private lazy val sr = s/( 2f * sqrt( 2f ) )

    /** convert to the screen coordinate from the camera coordinate. */
    def toScreen( p: Vector3 ) = {
      val v = Vector3.O -> p
      val d = v.abs
      v * ( sr / d )
    }

    /** convert to the camera coordinate from the screen coordinate. */
    def position( x: Int, y: Int ) = {
      val sx = screen.screenX( x )
      val sy = screen.screenY( y )
      val ss = s * s
      val rr = sx * sx + sy * sy
      val r = sqrt( rr )
      val theta = acos( 1f - 4 * rr / ss )
      //val phi = atan2( sy, sx )
      val xy = sr * sin( theta )
      if( x == screen.width/2 && y == screen.width/2 ) Vector3( 0, 0, sr * cos( theta ) )
      else Vector3( sx * xy / r, sy * xy / r, sr * cos( theta ) )
    }

  }

  object LambertCamera {

    /**
      * Create a camera instance with a z-direction and a rotation of camera.
      * @param screen A screen to be projected by this camera.
      * @param position A position of this camera in the current coordinates system.
      * @param rotation A rotation angle in radians of the direction from the x axis to the y axis about the 'direction' axis.
      * @param focalLength A focal length of this camera.
      */
    def apply( screen: Screen, position: Vector3, direction: Vector3, rotation: Float ): Camera = {
      val (nx, ny, nz) = Camera.fromDirection( direction, rotation )
      new LambertCamera( screen, position, nx, ny, nz )
    }

    def apply( screen: Screen, r: Float, theta: Float, phi: Float ): Camera = apply( screen, r, theta, phi, 0f )

    def apply( screen: Screen, r: Float, theta: Float, phi: Float, rotation: Float ): Camera = {
      val p = Camera.fromPolar( r, theta, phi )
      apply( screen, p, p -> Vector3.O, rotation )
    }

  }

}
