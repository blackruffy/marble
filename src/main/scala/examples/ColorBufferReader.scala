package examples {

  import net.ruffy.marble.{ math, graphics2d, graphics3d, Env }
  import math.{ DoubleEnv }
  import java.io.{ File }

  object ColorBufferReader extends Env with DoubleEnv {
 
    def extention( f: String ) = f.replaceAll(""".+\.(.+)""", "$1")

    def marge( in1: String, den1: Float, in2: String, den2: Float, npath: Int ) = {
      val ibuf1 = ColorBuffer( in1 )
      val ibuf2 = ColorBuffer( in2 )
      if( ibuf1.width != ibuf2.width )
        throw new Exception( "width is not same: %d != %d".format( ibuf1.width, ibuf2.width ) )
      if( ibuf1.height != ibuf2.height )
        throw new Exception( "height is not same: %d != %d".format( ibuf1.height, ibuf2.height) )
      val cbuf = ColorBuffer( ibuf1.width, ibuf1.height )
      ibuf1.foreach { (x, y, a) =>
        val r1 = den1/npath
        val r2 = den2/npath
        cbuf.update( x, y, a * r1 + ibuf2(x, y) * r2 )
      }
      cbuf
    }

    def main( args: Array[String] ) {

      args.head match {

        case "marge" => args.tail match {
          case Array( in1, r1, in2, r2, n, out ) => marge( in1, r1.toFloat, in2, r2.toFloat, n.toInt ).write( extention( out ), out )
          case _ => println("invalid arguments")
        }

        case _ => println("invalid command")

      }

    }

  }

}
