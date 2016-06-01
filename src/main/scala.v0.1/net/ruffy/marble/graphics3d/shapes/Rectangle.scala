package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math._
  import net.ruffy.marble._
  import graphics2d._
  import graphics3d._

  class Rectangle(
    val p0: Vector3, val n0: Vector3,
    val p1: Vector3, val n1: Vector3,
    val p2: Vector3, val n2: Vector3,
    val p3: Vector3, val n3: Vector3,
    material: Material
  ) extends Polygons( List(p0, p1, p2, p3), List(n0, n1, n2, n3), material, Polygons.toTriangles ) {

    def create( points: List[Vector3], normals: List[Vector3], material: Material ) = (points, normals) match {
      case (List(p0, p1, p2, p3), List(n0, n1, n2, n3)) => new Rectangle(p0, n0, p1, n1, p2, n2, p3, n3, material )
    }

    override def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = if( normal.z < 0 ) {
      val screen = camera.screen
      val (x0, y0, z0) = screen.pixel( camera.toScreen( p0 ) )
      val (x1, y1, z1) = screen.pixel( camera.toScreen( p1 ) )
      val (x2, y2, z2) = screen.pixel( camera.toScreen( p2 ) )
      val (x3, y3, z3) = screen.pixel( camera.toScreen( p3 ) )
      val mtype = if( Math.equals(material.surface.emittance)(1f) ) Material.Emittance else Material.Diffusivity
      val color = lights.foldLeft[Color]( Color.Black ) { case ( d, l ) => d + l.color( normal, center, material, mtype ) }.toInt
      g.fillTriangle( x0, y0, x1, y1, x2, y2, color )
      g.fillTriangle( x2, y2, x3, y3, x0, y0, color )
    }


  }

  object Rectangle {

    def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3 ): Rectangle = Rectangle( p0, p1, p2, p3, Material.Default )

    def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3, material: Material ): Rectangle = {
      val n = Triangle.normal( p0, p1, p2 )
      new Rectangle( p0, n, p1, n, p2, n, p3, n, material )
    }

    def apply(
      p0: Vector3, n0: Vector3,
      p1: Vector3, n1: Vector3,
      p2: Vector3, n2: Vector3,
      p3: Vector3, n3: Vector3,
      material: Material
    ): Rectangle = new Rectangle( p0, n0, p1, n1, p2, n2, p3, n3, material )

    def apply(
      p0: Vector3, n0: Vector3,
      p1: Vector3, n1: Vector3,
      p2: Vector3, n2: Vector3,
      p3: Vector3, n3: Vector3
    ): Rectangle = new Rectangle( p0, n0, p1, n1, p2, n2, p3, n3, Material.Default )

    def apply( width: Float, height: Float, material: Material ): Rectangle = Rectangle(
      Vector3( -width/2,  height/2, 0),
      Vector3(  width/2,  height/2, 0),
      Vector3(  width/2, -height/2, 0),
      Vector3( -width/2, -height/2, 0),
      material
    )

    def apply( width: Float, height: Float ): Rectangle = Rectangle(
      Vector3( -width/2,  height/2, 0),
      Vector3(  width/2,  height/2, 0),
      Vector3(  width/2, -height/2, 0),
      Vector3( -width/2, -height/2, 0),
      Material.Default
    )

  }

}
