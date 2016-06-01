package examples {

  import net.ruffy.util.Implicits._
  import net.ruffy.marble.graphics2d.{ Image, IntBuffer, ColorBuffer, ColorsBuffer, Color, Graphics }
  import net.ruffy.marble.graphics3d.{ Screen, Camera, Material, Branch, Leaf, PathTracer }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ WrappedFloat, Vector3, Matrix4x4 }
  import scala.collection.mutable.{ HashMap }

  object ColorsBufferReader {

    def extention( f: String ) = f.replaceAll(""".+\.(.+)""", "$1")

    def getColorBuffer( i: Int, width: Int, height: Int, map: HashMap[Int, Image[Color]] ): Image[Color] = map.get( i ) match {
      case Some( b ) => b
      case None =>
        val b = ColorBuffer( width, height )
        map.update( i, b )
        b
    }

    def main( args: Array[String] ) {

      args.head match {

        case "marge" => args.tail match {
          case Array( in, rate, _start, _end, out ) =>
            val start = _start.toInt
            val end = _end.toInt
            val num = end - start + 1
            val map = new HashMap[Int, Image[Color]]
            for( i <- start to end ) {
              println(">>>> %d".format(i))
              val ibuf = ColorsBuffer( in.format(i) )
              ibuf.foreach { (x, y, a) =>
                val size = a.size
                if( size > 0 ) {
                  val idx = size - 1
                  val c = a.foldLeft[Color]( Color.White ) { (x, y) => x * (y + Color(0.1f, 0.1f, 0.1f)).normalize }.normalize
                  val cc = c * (rate.toFloat/num.toFloat)// * ( if( 2 > 0 ) idx else 1 )

                  val cbuf = getColorBuffer( idx, ibuf.width, ibuf.height, map )
                  cbuf.update( _ + cc )(x, y)
                  val cbuf0 = getColorBuffer( 0, ibuf.width, ibuf.height, map )
                  cbuf0.update( _ + cc )(x, y)
                }
              }
            }
            map.keys.foreach { k => map.get(k).foreach { v =>
              v.write( extention(out), out.format(k) )
            } }
          case _ => println("invalid arguments")
        }

        case "print" => args.tail match {

          case Array( in ) =>
            ColorsBuffer( in ).foreach { (x, y, a) => print("%s,".format(a)) }

          case Array( "--with-pos", in ) =>
            ColorsBuffer( in ).foreach { (x, y, a) => println("(%d, %d): %d, %s".format(x, y, a.size, a)) }

          case _ => println("invalid arguments")
        }

        case _ => println("invalid command")
      }
    } // def main

  } // object IntBufferReader

}
