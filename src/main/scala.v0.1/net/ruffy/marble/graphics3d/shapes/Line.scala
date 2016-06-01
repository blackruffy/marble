package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.graphics2d.{ Color, Graphics, ZBuffer }
  import net.ruffy.marble.math.{ Vector3 }
  import net.ruffy.marble.graphics3d.{ Photon, Material, Camera }

  class Line( val p0: Vector3, val p1: Vector3, val color: Color ) extends Shape3D {

    type A = Line

    lazy val center = ( p0 + p1 ) / 2
    lazy val boundingSphere = BoundingSphere( center, (center -> p0).abs )
    lazy val material = Material.Default

    def inside_?( p: Vector3 ) = false

    def getIntersection( ray: Photon ) = None

    def setMaterial( material: Material ) = new Line( p0, p1, material.surface.diffuseColor )

    def transform( f: Vector3 => Vector3 ) = new Line( f( p0 ), f( p1 ), color )

    def draw( g: Graphics, camera: Camera ) = drawWireFrame( g, camera, color )

    def drawWireFrame( g: Graphics, camera: Camera, color: Int ) = {
      val (x0, y0, z0) = camera.screen.pixel( camera.toScreen( p0 ) )
      val (x1, y1, z1) = camera.screen.pixel( camera.toScreen( p1 ) )
      g.drawLine( x0, y0, x1, y1, color )
    }

    def fill( g: Graphics, camera: Camera, lights: List[Light] ) = drawWireFrame( g, camera, color )

    def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light], zbuf: ZBuffer ) = fill( g, camera, lights )

    lazy val randomPosition = center

    def createPhoton( color: Color ) = boundingSphere.createPhoton( color )

  }

}
