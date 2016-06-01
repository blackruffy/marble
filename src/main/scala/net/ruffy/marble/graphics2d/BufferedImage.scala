package net.ruffy.marble.graphics2d {

  import java.awt.image.{ BufferedImage => BI }
  import javax.imageio.{ ImageIO }
  import java.io.{ File }

  trait BufferedImageEnv {

    env: ColorEnv with ImageEnv =>

    class BufferedImage( val width: Int, val height: Int ) extends Image[Color] {

      private lazy val buf = new BI( width, height, BI.TYPE_INT_ARGB )

      def create( width: Int, height: Int ): Image[Color] = new BufferedImage( width, height )

      def apply( x: Int, y: Int ): Color = if( within(x, y) ) Color( buf.getRGB( x, y ) ) else Color.Black

      def update( x: Int, y: Int, c: Color ): Unit = if( within(x, y) ) buf.setRGB( x, y, c.toInt )

      def update( x: Int, y: Int, c: Int ): Unit = if( within(x, y) ) buf.setRGB( x, y, c )

      def update( f: Color => Color )( x: Int, y: Int ): Unit = if( within(x, y) ) update( x, y, f( apply( x, y ) ) )

      def write( file: String ): Unit = write( "png", file )

      def write( format: String, file: String ): Unit = ImageIO.write(buf, format, new File(file))

    }

    object BufferedImage {

      def apply( width: Int, height: Int ): BufferedImage = new BufferedImage( width, height )

    }

  }

}
