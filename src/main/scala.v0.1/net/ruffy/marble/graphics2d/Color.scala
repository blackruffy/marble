package net.ruffy.marble.graphics2d {

  import net.ruffy.marble.math.Math

  /**
    * @param alpha 不透明度。1が不透明。0が透明。
    */
  class Color( val alpha: Float, val red: Float, val green: Float, val blue: Float, normalized_? : Boolean ) {

    private lazy val abs2 = red * red + green * green + blue * blue

    private lazy val abs = Math.sqrt( abs2 )

    private lazy val max = red max green max blue

    lazy val normalize = if( normalized_? ) this else new Color( alpha, red/max, green/max, blue/max, true )

    lazy val toInt: Int = {
      (cut(alpha*255) << 24) | (cut(red*255) << 16) | (cut(green*255) << 8) | cut(blue*255)
    }

    lazy val avg: Float = (red + green + blue)/3

    def + ( c: Color ) = Color( red + c.red, green + c.green, blue + c.blue )

    def - ( c: Color ) = Color( red - c.red, green - c.green, blue - c.blue )

    def * ( c: Color ): Color = Color( red * c.red, green * c.green, blue * c.blue )

    def * ( x: Int ): Color = Color( red * x, green * x, blue * x )

    def * ( x: Float ): Color = Color( red * x, green * x, blue * x )

    def / ( c: Color ): Color = Color( red / c.red, green / c.green, blue / c.blue )

    def / ( x: Int ) = Color( red / x, green / x, blue / x )

    def / ( x: Float ) = Color( red / x, green / x, blue / x )

    def map( func: Float => Float ): Color = Color( func( alpha ), func( red ), func( green ), func( blue ) )

    private def cut( x: Float ): Int = (if( x < 0 ) 0 else if( x > 255 ) 255 else x).toInt

    def complement = Color( 1f - alpha, 1f - red, 1f - green, 1f - blue )

    override def toString = "Color(%f, %f, %f, %f)".format( alpha, red, green, blue )

  }


  object Color {

    object  Black   extends Color( 1.0f, 0.0f, 0.0f, 0.0f, false )
    object  White   extends Color( 1.0f, 1.0f, 1.0f, 1.0f, false )
    object  Red     extends Color( 1.0f, 1.0f, 0.0f, 0.0f, false )
    object  Green   extends Color( 1.0f, 0.0f, 1.0f, 0.0f, false )
    object  Blue    extends Color( 1.0f, 0.0f, 0.0f, 1.0f, false )
    object  Yellow  extends Color( 1.0f, 1.0f, 1.0f, 0.0f, false )
    object  Cyan    extends Color( 1.0f, 0.0f, 1.0f, 1.0f, false )
    object  Magenta extends Color( 1.0f, 1.0f, 0.0f, 1.0f, false )

    implicit def colorToInt( c: Color ): Int = c.toInt

    def red( c: Int ): Int = c >> 16 & 0xff
    def green( c: Int ): Int = c >> 8 & 0xff
    def blue( c: Int ): Int = c & 0xff
    def alpha( c: Int ): Int = c >> 24 & 0xff

    def apply( alpha: Float, red: Float, green: Float, blue: Float ): Color = new Color( alpha, red, green, blue, false )

    def apply( red: Float, green: Float, blue: Float ): Color = Color( 1.0f, red, green, blue )

    def apply( c: Int ): Color = Color( alpha( c ) / 255f, red( c ) / 255f, green( c ) / 255f, blue( c ) / 255f )

    def unapply( c: Color ): Option[(Float, Float, Float, Float)] = Some( c.alpha, c.red, c.green, c.blue )

  }

}
