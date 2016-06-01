package net.ruffy.marble.graphics3d.renderer {

  import net.ruffy.marble.graphics2d.{ ColorEnv }
  import net.ruffy.marble.math.{ MathEnv, NumberEnv }

  trait MaterialEnv {

    env: ColorEnv with MathEnv with NumberEnv =>

    class Material( val surface: Material.Surface, val medium: Material.Medium )

    object Material {

      case class Medium( refractiveIndex: Num )

      object Medium {
        lazy val Default = Medium( 1f )
      }

      abstract class Type
      case object Diffusivity extends Type
      case object Reflectance extends Type
      case object Refractive  extends Type
      case object Emittance   extends Type

      //val default = Diffuse( Color.Black )

      def apply( surface: Surface, medium: Medium ): Material = new Material( surface, medium )
      def apply( surface: Surface ): Material = new Material( surface, Medium.Default )

      /**
        * 物体の表面を表現するクラス。
        */
      class Surface(
        _diffusivity       : Num,
        val diffuseColor   : Color,
        _reflectance       : Num,
        val reflectiveColor: Color,
        _refractiveRate    : Num,
        val refractiveColor: Color,
        _emittance         : Num,
        val emissionColor  : Color
      ) {

        private lazy val sum = _diffusivity + _reflectance + _refractiveRate + _emittance

        lazy val diffusivity     = _diffusivity    / sum
        lazy val reflectance     = _reflectance    / sum
        lazy val refractiveRate  = _refractiveRate / sum
        lazy val emittance       = _emittance      / sum

        private lazy val diffusivity_ = diffusivity
        private lazy val reflectance_ = diffusivity_ + reflectance
        private lazy val refractive_  = reflectance_ + refractiveRate
        private lazy val emittance_   = refractive_  + emittance

        lazy val isLight = emittance > 0f

        lazy val rates = (diffusivity, reflectance, refractiveRate, emittance)

        def color( t: Material.Type ) = t match {
          case Diffusivity => diffuseColor
          case Reflectance => reflectiveColor
          case Refractive  => refractiveColor
          case Emittance   => emissionColor
        }

        /**
          * 指数から表面のタイプをランダムに選ぶ。
          */
        def random: Material.Type = {
          val r = Math.random
          if( r <= diffusivity_ )                          Material.Diffusivity
          else if( diffusivity_ < r && r <= reflectance_ ) Material.Reflectance
          else if( reflectance_ < r && r <= refractive_  ) Material.Refractive
          else                                             Material.Emittance
        }

        override lazy val toString: String = "Surface(%f, %s, %f, %s, %f, %s, %f, %s)".format(
          diffusivity, diffuseColor,
          reflectance, reflectiveColor,
          refractiveRate, refractiveColor,
          emittance, emissionColor
        )

      } // class Surface

      object Surface {

        def apply(
          diffusivity    : Num,
          diffuseColor   : Color,
          reflectance    : Num,
          reflectiveColor: Color,
          refractiveRate : Num,
          refractiveColor: Color,
          emittance      : Num,
          emissionColor  : Color
        ): Surface = new Surface(
          diffusivity,
          diffuseColor,
          reflectance,
          reflectiveColor,
          refractiveRate,
          refractiveColor,
          emittance,
          emissionColor
        )

        def apply(
          diffusivity: Option[(Num, Color)],
          reflectance: Option[(Num, Color)],
          refractive : Option[(Num, Color)],
          emittance  : Option[(Num, Color)]
        ): Surface = {
          val blank = (mkNum(0), Color.Black)
          val (diff, diffcol) = diffusivity.getOrElse(blank)
          val (refl, reflcol) = reflectance.getOrElse(blank)
          val (refr, refrcol) = refractive.getOrElse(blank)
          val (emit, emitcol) = emittance.getOrElse(blank)
          new Surface(
            diff, diffcol,
            refl, reflcol,
            refr, refrcol,
            emit, emitcol
          )
        }

      } // object Surface

      case class Diffusion( c: Color ) extends Material( Surface( Some(1f, c), None, None, None ), Medium.Default ) {
        override def toString = c.toString
      }

      case class Reflection( c: Color ) extends Material( Surface( None, Some(1f, c), None, None ), Medium.Default ) {
        override def toString = c.toString
      }

      case class Emitter( c: Color ) extends Material( Surface( None, None, None, Some(1f, c) ), Medium.Default ) {
        override def toString = c.toString
      }

      object WhiteLight extends Emitter( Color.White )

      lazy val Default = Diffusion( Color.Black )

    }

  }
}
