package net.ruffy.marble.graphics3d.shapes {

  import net.ruffy.marble.graphics2d.{ Color }
  import net.ruffy.marble.math.{ Vector3 }

  class Axis( direction: Vector3, color: Color ) extends Line( Vector3.O, direction, color )

  object Axis {
    lazy val X = new Axis( Vector3.Nx, Color.Red )
    lazy val Y = new Axis( Vector3.Ny, Color.Green )
    lazy val Z = new Axis( Vector3.Nz, Color.Blue )
  }
  

}
