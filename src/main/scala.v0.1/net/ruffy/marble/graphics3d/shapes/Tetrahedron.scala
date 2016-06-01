package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.math._
  import net.ruffy.marble.graphics3d._
  import Intersection.near

  class Tetrahedron( val tri0: Triangle, val tri1: Triangle, val tri2: Triangle, val tri3: Triangle, material: Material ) extends Solid.Impl( List( tri0, tri1, tri2, tri3 ), material )

  object Tetrahedron {
    def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3, material: Material ): Tetrahedron = new Tetrahedron(
      Triangle( p0, p1, p3, material ),
      Triangle( p0, p3, p2, material ),
      Triangle( p0, p2, p1, material ),
      Triangle( p1, p2, p3, material ),
      material
    )

    def apply( p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3 ): Tetrahedron = new Tetrahedron(
      Triangle( p0, p1, p3, Material.Default ),
      Triangle( p0, p3, p2, Material.Default ),
      Triangle( p0, p2, p1, Material.Default ),
      Triangle( p1, p2, p3, Material.Default ),
      Material.Default
    )
  }

}
