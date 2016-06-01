package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, Camera, Material, Branch, Leaf, PathTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }
  import java.io.{ File }

  object Scratch {

    def extention( f: String ) = f.replaceAll(""".+\.(.+)""", "$1")

    def marge( in1: String, den1: Float, in2: String, den2: Float, num: Int ) = {
      val ibuf1 = ColorBuffer( in1 )
      val ibuf2 = ColorBuffer( in2 )
      if( ibuf1.width != ibuf2.width )
        throw new Exception( "width is not same: %d != %d".format( ibuf1.width, ibuf2.width ) )
      if( ibuf1.height != ibuf2.height )
        throw new Exception( "height is not same: %d != %d".format( ibuf1.height, ibuf2.height) )
      val cbuf = ColorBuffer( ibuf1.width, ibuf1.height )
      val r1 = den1/num
      val r2 = den2/num
      ibuf1.foreach { (x, y, a) =>
        val col1 = a * r1
        val col2 = ibuf2( x, y ) * r2
        cbuf.update( x, y, col1 + col2 )
      }
      cbuf
    }

    def main( args: Array[String] ) {

      args.head match {

        case "marge" => args.tail match {
          case Array( in1, rate1, in2, rate2, num, out ) =>
            marge( in1, rate1.toFloat, in2, rate2.toFloat, num.toInt ).write( extention(out), out )

          case _ => println("invalid arguments")
        }

        case _ => println("invalid command")
      }
    } // def main

  } // object IntBufferReader

}
