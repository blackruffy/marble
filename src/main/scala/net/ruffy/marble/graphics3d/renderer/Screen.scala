package net.ruffy.marble.graphics3d.renderer {

  //import javafx.scene.image.WritableImage
  import net.ruffy.marble.graphics2d._
  import net.ruffy.marble.math._
  //import javafx.embed.swing._
  //import javax.imageio._

  trait ScreenEnv {

    env: ColorEnv
        with ImageEnv
        with MathEnv
        with NumberEnv
        with VectorEnv =>

    class Screen( val image: Image[Color] ) {

      val width = image.width
      val height = image.height
      //val graphics = new Graphics( image.getPixelWriter )
      val aspect: Num = mkNum( width )/mkNum( height )
      val xmin: Num = -aspect
      val xmax: Num = aspect
      val ymin: Num = -1
      val ymax: Num = 1
      val depth: Num = height

      def within( x: Int, y: Int ) = x >= 0 && x < width && y >= 0 && y < height

      def pixel( p: Vector3 ) = ( pixelX( p.x ), pixelY( p.y ), pixelZ( p.z ) )

      def pixelX( x: Num ): Int = ( (x - xmin)*( width/(xmax - xmin) ) ).toInt
      def pixelY( y: Num ): Int = ( (ymax - y)*( height/(ymax - ymin) ) ).toInt
      def pixelZ( z: Num ): Int = (z*depth/2).toInt

      def screenX( x: Int ): Num = (xmax - xmin)/width*x + xmin
      def screenY( y: Int ): Num = ymax - (ymax - ymin)/height*y
      def screenZ( z: Int ): Num = z*2/depth

      def addWith( f: (Color, Color) => Color )( x: Int, y: Int, c: Color ) = image.update( a => f( a, c ) )( x, y )

      def add( x: Int, y: Int, c: Color ) = image.update( a => a + c )( x, y )

      def write( format: String, file: String ): Unit = image.write( format, file )
    }

    object Screen {

      def apply( width: Int, height: Int ): Screen = new Screen( ColorBuffer( width, height ) )

    }

  }
}
