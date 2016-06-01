package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math._
  import net.ruffy.marble.graphics2d._
  import net.ruffy.marble.graphics3d._

  abstract class Light extends Shape3D {
    type A = Light

    val shape: Shape3D

    lazy val boundingSphere = shape.boundingSphere

    lazy val center = shape.center

    lazy val material = shape.material

    def inside_?( p: Vector3 ) = shape.inside_?( p )

    def create( shape: Shape3D ): Light

    def getIntersection( ray: Photon ) = shape.getIntersection( ray )

    def setMaterial( material: Material ) = create( shape.setMaterial( material ) )

    def transform( f: Vector3 => Vector3 ) = create( shape.transform( f ) )

    def drawWireFrame( g: Graphics, camera: Camera, color: Int ) = shape.drawWireFrame( g, camera, color )

    def fill( g: Graphics, camera: Camera, lights: List[Light] ) = shape.fill( g, camera, lights )

    def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ) = shape.fillWithGouraudShading( g, camera, lights, zbuf )

    def randomPosition = shape.randomPosition

    def createPhoton( color: Color ): Photon = shape.createPhoton( color )

    def color( t: Material.Type, i: Intersection ): Color = color( i.normal, i.point, i.shape.material, t )

    /**
      * レイトレース時に使用するレイとの交差点での色を計算する。
      * @param n normal on the object
      * @param p point on the object
      * @param m material of the object
      * @param a type of material of the object
      */
    def color( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color

  }

  object Light {

    val attenuation = 1.0f
    val lambertFactor = 0.0f
    val photonPower = 1.0f

    class Rectangle( val shape: Shape3D ) extends Light {

      lazy val normal: Vector3 = shape.asInstanceOf[net.ruffy.marble.graphics3d.shapes.Polygons].normal

      def create( shape: Shape3D ) = new Rectangle( shape )

      def color( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
        case Material.Emittance => m.surface.emissionColor
        case _ => 
          val lp = (center -> p).normalize
          val s = (normal * lp) * ( (-lp) * n )
          val t: Float = if( s < 0 ) 0f else s
          m.surface.color(a) * t
      }
    }

    object Rectangle {

      def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3 ): Rectangle = new Rectangle( net.ruffy.marble.graphics3d.shapes.Rectangle( p0, p1, p2, p3, Material.WhiteLight ) )

    }

    class Sphere ( val shape: Shape3D ) extends Light {

      def create( shape: Shape3D ) = new Sphere( shape )

      def color( n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
        case Material.Emittance => m.surface.emissionColor
        case _ =>  
          val s = n * (p -> center).normalize
          val t: Float = if( s < 0 ) 0f else s
          m.surface.color(a) * t
      }
    }

    object Sphere {

      def apply( center: Vector3, radius: Float ): Sphere = new Sphere( BoundingSphere( center, radius, Material.WhiteLight ) )

    }

    def color( t: Material.Type, i: Intersection ): Color = i.shape.material.surface.color( t )

//    def getColor( light: Shape3D )( t: Material.Type, i: Intersection ): Color = getColor( light, i.normal, i.point, i.material, t )
//
//    def getColor( light: Shape3D, n: Vector3, p: Vector3, m: Material, a: Material.Type ): Color = a match {
//      case Material.Emittance => m.color
//      case _ => 
//        val lv = (p -> light.center).normalize
//        //val lv = -Vector3.Ny
//        //val nn = (light.center -> p).normalize
//        val s = n * lv
//        //println( ">>> "+n+", "+lv+", "+s )
//        //println( ">>> "+(n angle lv)+", "+s )
//        val t: Float = if( s < 0 ) 0f else s
//        //println( "+++ "+(m.color *t))
//        m.color * t
//    }

  }

}
