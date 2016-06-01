package net.ruffy.marble.graphics2d {

  import net.ruffy.marble.math.{ MathEnv, NumberEnv }

  trait ColorEnv {

    env: MathEnv with NumberEnv =>

    /**
      * @param alpha 不透明度。1が不透明。0が透明。
      */
    class Color( val alpha: Num, val red: Num, val green: Num, val blue: Num, normalized_? : Boolean ) {

      private lazy val abs2 = red * red + green * green + blue * blue

      private lazy val abs = Math.sqrt( abs2 )

      private lazy val max = red max green max blue

      //lazy val normalize = if( normalized_? ) this else new Color( alpha, red/max, green/max, blue/max, true )
      lazy val normalize = if( normalized_? ) this else new Color( alpha, red/abs, green/abs, blue/abs, true )

      lazy val toInt: Int = {
        (cut(alpha*255) << 24) | (cut(red*255) << 16) | (cut(green*255) << 8) | cut(blue*255)
      }

      lazy val avg: Num = (red + green + blue)/3

      def + ( c: Color ) = Color( red + c.red, green + c.green, blue + c.blue )

      def - ( c: Color ) = Color( red - c.red, green - c.green, blue - c.blue )

      def * ( c: Color ): Color = Color( red * c.red, green * c.green, blue * c.blue )

      def * ( x: Int ): Color = Color( red * x, green * x, blue * x )

      def * ( x: Num ): Color = Color( red * x, green * x, blue * x )

      def / ( c: Color ): Color = Color( red / c.red, green / c.green, blue / c.blue )

      def / ( x: Int ) = Color( red / x, green / x, blue / x )

      def / ( x: Num ) = Color( red / x, green / x, blue / x )

      def map( func: Num => Num ): Color = Color( func( alpha ), func( red ), func( green ), func( blue ) )

      private def cut( x: Num ): Int = (if( x < Math._0 ) Math._0 else if( x > 255 ) env.mkNum(255) else x).toInt

      def complement = Color( Math._1 - alpha, Math._1 - red, Math._1 - green, Math._1 - blue )

      override def toString = "Color(%f, %f, %f, %f)".format( alpha.underlying, red.underlying, green.underlying, blue.underlying )

    }


    object Color {

      object  Black   extends Color( 1.0, 0.0, 0.0, 0.0, false )
      object  White   extends Color( 1.0, 1.0, 1.0, 1.0, false )
      object  Red     extends Color( 1.0, 1.0, 0.0, 0.0, false )
      object  Green   extends Color( 1.0, 0.0, 1.0, 0.0, false )
      object  Blue    extends Color( 1.0, 0.0, 0.0, 1.0, false )
      object  Yellow  extends Color( 1.0, 1.0, 1.0, 0.0, false )
      object  Cyan    extends Color( 1.0, 0.0, 1.0, 1.0, false )
      object  Magenta extends Color( 1.0, 1.0, 0.0, 1.0, false )

      implicit def colorToInt( c: Color ): Int = c.toInt

      def red( c: Int ): Int = c >> 16 & 0xff
      def green( c: Int ): Int = c >> 8 & 0xff
      def blue( c: Int ): Int = c & 0xff
      def alpha( c: Int ): Int = c >> 24 & 0xff

      def apply( alpha: Num, red: Num, green: Num, blue: Num ): Color = new Color( alpha, red, green, blue, false )

      def apply( red: Num, green: Num, blue: Num ): Color = Color( 1.0, red, green, blue )

      def apply( c: Int ): Color = Color( alpha( c ) / 255, red( c ) / 255, green( c ) / 255, blue( c ) / 255 )

      def unapply( c: Color ): Option[(Num, Num, Num, Num)] = Some( c.alpha, c.red, c.green, c.blue )

    }

  }
}
