package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.marble.graphics3d.renderer._
  import net.ruffy.marble.math._

  trait BoxEnv {

    env: RectangleEnv
        with IntersectionEnv
        with SolidEnv
        with Shape3DEnv
        with MaterialEnv
        with PolygonEnv
        with VectorEnv
        with MathEnv
        with NumberEnv =>

    import Intersection.near

    class Box(
      val front   : Rectangle,
      val top     : Rectangle,
      val left    : Rectangle,
      val right   : Rectangle,
      val rear    : Rectangle,
      val bottom  : Rectangle,
      material    : Material
    ) extends Solid( List( front, top, left, right, rear, bottom ), material ) {
      type A = Box

      val p0 = front.p0
      val p1 = front.p1
      val p2 = front.p2
      val p3 = front.p3

      val p4 = rear.p0
      val p5 = rear.p1
      val p6 = rear.p2
      val p7 = rear.p3

      def create( polygons: List[Polygon], material: Material ) = polygons match {
        case List(
          p0: Rectangle,
          p1: Rectangle,
          p2: Rectangle,
          p3: Rectangle,
          p4: Rectangle,
          p5: Rectangle
        ) => new Box( p0, p1, p2, p3, p4, p5, material )
        case _ => throw new Exception("invalid number of arguments to create box")
      }

    }

    object Box {

      def apply(
        p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3,
        p4: Vector3, p5: Vector3, p6: Vector3, p7: Vector3,
        material: Material
      ): Box = new Box(
        Rectangle( p0, p3, p2, p1, material ), // front
        Rectangle( p0, p1, p5, p4, material ), // top
        Rectangle( p0, p4, p7, p3, material ), // left
        Rectangle( p1, p2, p6, p5, material ), // right
        Rectangle( p4, p5, p6, p7, material ), // rear
        Rectangle( p3, p7, p6, p2, material ), // bottom
        material
      )

      def apply( width: Num, height: Num, depth: Num, material: Material ): Box = Box(
        Vector3( -width/2f,  height/2,  depth/2 ),
        Vector3(  width/2f,  height/2,  depth/2 ),
        Vector3(  width/2f, -height/2,  depth/2 ),
        Vector3( -width/2f, -height/2,  depth/2 ),

        Vector3( -width/2f,  height/2, -depth/2 ),
        Vector3(  width/2f,  height/2, -depth/2 ),
        Vector3(  width/2f, -height/2, -depth/2 ),
        Vector3( -width/2f, -height/2, -depth/2 ),

        material
      )

      def apply( width: Num, height: Num, depth: Num ): Box = Box(
        Vector3( -width/2f,  height/2,  depth/2 ),
        Vector3(  width/2f,  height/2,  depth/2 ),
        Vector3(  width/2f, -height/2,  depth/2 ),
        Vector3( -width/2f, -height/2,  depth/2 ),

        Vector3( -width/2f,  height/2, -depth/2 ),
        Vector3(  width/2f,  height/2, -depth/2 ),
        Vector3(  width/2f, -height/2, -depth/2 ),
        Vector3( -width/2f, -height/2, -depth/2 ),

        Material.Default
      )

    }


  }
}
