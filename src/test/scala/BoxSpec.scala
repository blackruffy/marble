
import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import net.ruffy.marble._
import graphics3d._
import graphics2d._
import graphics3d.shapes._
import util._
import math._
import WrappedFloat.toWrappedFloat
import graphics2d.Color.colorToInt

class BoxSpec extends FlatSpec with MustMatchers {

  "box" should "fill" in {

    val image = ColorBuffer( 200, 200 )

    val screen = new Screen( image )

    val g = Graphics( image )

    val camera = Camera( screen, 5f, 45f.toRadians, 45f.toRadians, 5.0f )

    val box = Box( 1f, 1f, 1f, Material.Diffuse( Color.Blue ) ).toCamera( camera )
    val xaxis = Axis.X.toCamera( camera )
    val yaxis = Axis.Y.toCamera( camera )
    val zaxis = Axis.Z.toCamera( camera )

    val lights:List[Light] = List( Light.Sphere( Vector3(2f, 3f, 1f), 0.1f ).toCamera( camera ) )

    box.fill( g, camera, lights )
    box.drawWireFrame( g, camera, Color.White )
    xaxis.draw( g, camera )
    yaxis.draw( g, camera )
    zaxis.draw( g, camera )

    //g.drawLine( 0, 0, 500, 500, Color.White )

    image.write( "png", "image/box.png" )

  }

}
