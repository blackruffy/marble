package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ IntBuffer, ColorBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, Camera, Material, Branch, Leaf, PathTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }
  import java.io.{ File }

  object IntBufferReader {

    def extention( f: String ) = f.replaceAll(""".+\.(.+)""", "$1")

    def marge( in1: String, den1: Float, in2: String, den2: Float ) = {
      val ibuf1 = IntBuffer( in1 )
      val ibuf2 = IntBuffer( in2 )
      if( ibuf1.width != ibuf2.width )
        throw new Exception( "width is not same: %d != %d".format( ibuf1.width, ibuf2.width ) )
      if( ibuf1.height != ibuf2.height )
        throw new Exception( "height is not same: %d != %d".format( ibuf1.height, ibuf2.height) )
      val cbuf = ColorBuffer( ibuf1.width, ibuf1.height )
      ibuf1.foreach { (x, y, a) =>
        val col1 = if( a == -1 ) 1 else a.toFloat*den1
        val col2 = ibuf2( x, y ) |> { b => if( b == -1 ) 1 else b.toFloat*den2 }
        val col = (col1 + col2) |> { x => if( x > 1f ) 1f else x }
        cbuf.update( x, y, Color( col, col, col ) )
      }
      cbuf
    }

    def main( args: Array[String] ) {

      args.head match {

        case "conv" => args.tail match {

          case Array( in, den, cnt, out ) =>
            val ibuf = IntBuffer( in )
            val cbuf = ColorBuffer( ibuf.width, ibuf.height )
            ibuf.foreach { (x, y, a) =>
              val col = (if( a == -1 ) 1f else a.toFloat*den.toFloat/cnt.toFloat) |> { x => if( x > 1f ) 1f else x }
              cbuf.update( x, y, Color( col, col, col ) )
            }
            cbuf.write( extention( out ), out )

          case Array( in1, den1, in2, den2, out ) =>
            val cbuf = marge( in1, den1.toFloat,  in2, den2.toFloat )
            cbuf.write( extention( out ), out )

          case Array( in1, den1, in2, den2, start, end, out ) =>
            for( i <- start.toInt to end.toInt ) {
              println( "%d: %s".format(i, out.format( i )) )
              val cbuf = marge( in1.format( i ), den1.toFloat/i, in2.format( i ), den2.toFloat/i )
              cbuf.write( extention( out ), out.format( i ) )
            }

          case _ => println("invalid arguments")
        }

        case "print" => args.tail match {

          case Array( in ) =>
            IntBuffer( in ).foreach { (x, y, a) => print("%d,".format(a)) }

          case Array( "--with-pos", in ) =>
            IntBuffer( in ).foreach { (x, y, a) => println("(%d, %d): %d".format(x, y, a)) }

          case _ => println("invalid arguments")
        }

        case "for-stat" => args.tail match {
          case Array( dir, fname ) =>
            var cnt = 0
            (new File(dir)).list().filter( _.matches( fname ) ).foreach { f =>
              cnt += 1
              val path = "%s/%s".format(dir, f)
              val b = IntBuffer( path )
              for( x <- 0 until 150; y <- 250 until 251 ) {
                println("%d\t%d\t%d".format( cnt, x, b(x, y) ))
              }
            }

          case _ => println("invalid arguments")
        }

        case _ => println("invalid command")
      }
    } // def main

  } // object IntBufferReader

}
