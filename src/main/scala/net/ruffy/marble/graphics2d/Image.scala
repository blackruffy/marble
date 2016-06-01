package net.ruffy.marble.graphics2d {

  import net.ruffy.marble.math.{ MathEnv, NumberEnv }

  trait ImageEnv {

    env: ColorEnv with MathEnv with NumberEnv =>

    import javax.imageio._
    import java.io._
    import java.awt.image.BufferedImage
    import java.nio._
    import net.ruffy.util.Implicits._

    class WrappedByteBuffer( buf: ByteBuffer ) {
      def getNum: Num = getNumByteBuf( buf )
      def putNum( v: Num ): Unit = putNumByteBuf( buf, v )
    }

    implicit def toWrappedByteBuffer(
      buf: ByteBuffer
    ): WrappedByteBuffer = new WrappedByteBuffer( buf )

    abstract class Image[A] {
      val width: Int
      val height: Int

      lazy val zbuffer = new ZBuffer( width, height )

      protected def create: Image[A] = create( width, height )

      def create( width: Int, height: Int ): Image[A]

      def within(
        x: Int,
        y: Int
      ) = x >= 0 && x < width && y >= 0 && y < height

      def apply( x: Int, y: Int ): A

      def update( x: Int, y: Int, a: A ): Unit

      def update( x: Int, y: Int, a: Int ): Unit

      def update( f: A => A )( x: Int, y: Int ): Unit

      /**
        * バイナリファイルを出力
        */
      def write( file: String ): Unit

      /**
        * 画像ファイルを出力
        */
      def write( format: String, file: String ): Unit

      def foreach( f: (Int, Int, A) => Unit ): Unit = for {
        x <- 0 until width
        y <- 0 until height
      } { f( x, y, apply( x, y ) ) }

      def map( f: (Int, Int, A) => A ): Image[A] = {
        val img = create
        for {
          x <- 0 until width
          y <- 0 until height
        } {
          img.update( x, y, f( x, y, apply( x, y ) ) )
        }
        img
      }

      def update( f: (Int, Int, A) => A ): Unit = for {
        x <- 0 until width
        y <- 0 until height
      } update( c => f( x, y, c ) )( x, y )

    }

    object Image {

      /** 複数の画像を結合する。*/
      def apply[A](
        imgs: List[List[Image[A]]],
        create: (Int, Int) => Image[A]
      ): Image[A] = {
        val (w, h) = imgs.foldLeft( (0, 0) ) { case ((w, h), xs) =>
          val (mw, mh) = xs.foldLeft( (0, 0) ) { case ((w, h), img) =>
            (
              w + img.width,
              if( img.height > h ) img.height else h
            )
          }
          ( if( mw > w ) mw else w, h + mh )
        }
        //println("===> %d, %d".format( w, h))
        val img = create( w, h )
        var cy = 0
        imgs.foreach { xs =>
          var cx = 0
          val mh = xs.foldLeft( 0 ) { (h, i) =>
            i.foreach { (x, y, a) =>
              //println("%d, %d, %s".format(cx + y, cy + y, a))
              img.update(cx + x, cy + y, a)
            }
            cx += i.width
            if( i.height > h ) i.height else h
          }
          cy += mh
        }
        img
      }

      def apply[A](
        imgs: List[List[Image[A]]],
        img: Image[A]
      ): Unit = apply( imgs, (w, h) => {
        if( w != img.width ) throw new Exception("invalid image width")
        if( h != img.height ) throw new Exception("invalid image height")
        img
      } )

      /**
        * A型のバッファ。
        */
      abstract class Type[A]( underlying: ByteBuffer ) {
        val blank: A
        def get: A
        def put( a: A )
        /** 画像を生成するときにColorに変換する */
        def color( a: A ): Color
        def color: Color = color( get )
        /** BufferをInt型で更新するときに使う */
        def convert( x: Int ): A
      }

      /**
        * 色情報を保持するバッファ。
        */
      class ColorType( underlying: ByteBuffer ) extends Type[Color]( underlying ) {
        val blank = Color.Black
        def get = Color(
          underlying.getNum,
          underlying.getNum,
          underlying.getNum,
          underlying.getNum
        )
        def put( c: Color ) = {
          underlying.putNum( c.alpha )
          underlying.putNum( c.red )
          underlying.putNum( c.green )
          underlying.putNum( c.blue )
        }
        def color( c: Color ) = c
        def convert( x: Int ) = Color( x )
      }

      object ColorType {
        def wrap( buf: ByteBuffer ): ColorType = new ColorType( buf )
      }

      /**
        * Int型を保持するバッファ
        */
      class IntType( underlying: ByteBuffer ) extends Type[Int]( underlying ) {
        val blank = 0
        def get = underlying.getInt
        def put( x: Int ) = underlying.putInt( x )
        def color( x: Int ) = Color( x )
        def convert( x: Int ) = x
      }

      object IntType {
        def wrap( buf: ByteBuffer ): IntType = new IntType( buf )
      }

      /**
        * 色の配列を保持するバッファ。
        */
      class Colors( underlying: ByteBuffer ) extends Type[Seq[Color]]( underlying ){
        val blank = Nil
        def get = {
          val n = underlying.getInt
          for( i <- 0 until n ) yield {
            Color( underlying.getFloat, underlying.getFloat, underlying.getFloat, underlying.getFloat )
          }
        }
        def put( x: Seq[Color] ) = {
          val n = x.size
          underlying.putInt( x.size )
          x.foreach { c =>
            underlying.putNum( c.alpha )
            underlying.putNum( c.red )
            underlying.putNum( c.green )
            underlying.putNum( c.blue )
          }
        }
        def color( x: Seq[Color] ): Color = x.head
        def convert( x: Int ): Seq[Color] = Color( x ) :: Nil
      }

      object Colors {
        def wrap( buf: ByteBuffer ) : Colors = new Colors( buf )
      }

    }

    /**
      * Image.Type[A]型のピクセル情報を保持するバッファ。
      * @param width
      * @param height
      * @param buf
      * @param typeSize Image.Type[A]型のサイズ
      */
    class ImageBuffer[A] protected ( val width: Int, val height: Int, buf: ByteBuffer, typeSize: Int, wrap: ByteBuffer => Image.Type[A] ) extends Image[A] {

      private lazy val wrapper = wrap( buf )

      //protected def create: Image[A] = ImageBuffer( width, height, typeSize, wrap )

      def create( width: Int, height: Int ): Image[A] = ImageBuffer( width, height, typeSize, wrap )

      private def getPosition( x: Int, y: Int ): Int = (x + y * width) * typeSize + ImageBuffer.headerSize

      def position( x: Int, y: Int ): Unit = getPosition(x, y) |> buf.position

      def get:A  = wrapper.get

      def apply( x: Int, y: Int ): A = if( within(x, y) )  {
        position( x, y )
        get
      } else wrapper.blank

      def update( a: A ): Unit = {
        wrapper.put( a )
      }

      def update( x: Int, y: Int, a: A ): Unit = if( within(x, y) ) {
        position( x, y )
        update( a )
      }

      def update( x: Int, y: Int, a: Int ): Unit = update( x, y, wrapper.convert( a ) )

      def update( f: A => A )( x: Int, y: Int ): Unit = if( within(x, y) ) {
        position( x, y )
        buf.mark
        val a = f( get )
        buf.reset
        update( a )
      }

      def toBufferedImage: BufferedImage = {
        val bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB )
        position( 0, 0 )
        for( y <- 0 until height; x <- 0 until width ) {
          val c = wrapper.color.toInt
          bi.setRGB( x, y, if( c == 0 ) 0xff000000 else c )
        }
        bi
      }

      def write( file: String ): Unit = {
        val ch = (new FileOutputStream( file )).getChannel
        buf.position( 0 )
        ch.write( buf )
        ch.close
      }

      def write( format: String, file: String ): Unit = {
        ImageIO.write(toBufferedImage, format, new File(file))
      }

    }

    object ImageBuffer {

      /** ヘッダーサイズ。width(4bytes), height(4bytes), typeSize(4bytes) */
      val headerSize = 12

      private def bufferSize( width: Int, height: Int, typeSize: Int ) = headerSize + width * height * typeSize

      private def createEmptyBuffer( width: Int, height: Int, typeSize: Int ): ByteBuffer = ByteBuffer.allocateDirect( bufferSize( width, height, typeSize ) )

      private def readSize( file: String ): (Int, Int, Int) = {
        val tmp = ByteBuffer.allocateDirect( headerSize )
        val is = new FileInputStream( file ).getChannel
        is.read( tmp )
        tmp.position( 0 )
        val width = tmp.getInt
        val height = tmp.getInt
        val typeSize = tmp.getInt
        is.close
        ( width, height, typeSize )
      }

      private def apply[A]( width: Int, height: Int, buf: ByteBuffer, typeSize: Int, wrap: ByteBuffer => Image.Type[A] ): ImageBuffer[A] = new ImageBuffer( width, height, buf, typeSize, wrap )

      def apply[A]( width: Int, height: Int, typeSize: Int, wrap:ByteBuffer => Image.Type[A] ): ImageBuffer[A] =  {
        val buf = createEmptyBuffer( width, height, typeSize )
        buf.putInt( width )
        buf.putInt( height )
        buf.putInt( typeSize )
        apply( width, height, buf, typeSize, wrap )
      }

      def apply[A]( file: String, wrap: ByteBuffer => Image.Type[A] ): ImageBuffer[A] = {
        val ( width, height, typeSize ) = readSize( file )
        val input = new FileInputStream( file ).getChannel
        val buf = createEmptyBuffer( width, height, typeSize )
        input.read( buf )
        input.close
        apply( width, height, buf, typeSize, wrap )
      }


    }

    class IntBuffer private ( width: Int, height: Int, buf: ByteBuffer ) extends ImageBuffer[Int]( width, height, buf, IntBuffer.typeSize, Image.IntType.wrap )

    object IntBuffer {

      /** Int型のサイズ */
      val typeSize = 4

      def apply( width: Int, height: Int ): Image[Int] = ImageBuffer( width, height, typeSize, Image.IntType.wrap )
      def apply( file: String ): Image[Int] = ImageBuffer( file, Image.IntType.wrap )
    }

    class ColorBuffer private ( width: Int, height: Int, buf: ByteBuffer ) extends ImageBuffer[Color]( width, height, buf, ColorBuffer.typeSize, Image.ColorType.wrap )

    object ColorBuffer {

      /** Color型のサイズ */
      val typeSize = 4 * numSize

      def apply( width: Int, height: Int ): Image[Color] = ImageBuffer( width, height, typeSize, Image.ColorType.wrap )
      def apply( file: String ): Image[Color] = ImageBuffer( file, Image.ColorType.wrap )

    }

    class ColorsBuffer private ( width: Int, height: Int, buf: ByteBuffer, typeSize: Int ) extends ImageBuffer[Seq[Color]]( width, height, buf, typeSize, Image.Colors.wrap )

    object ColorsBuffer {
      def apply( width: Int, height: Int, typeSize: Int ): Image[Seq[Color]] = ImageBuffer( width, height, typeSize, Image.Colors.wrap )
      def apply( file: String ): Image[Seq[Color]] = ImageBuffer( file, Image.Colors.wrap )
      def typeSize( ntrace: Int ): Int = (ntrace+3)*16 + 4
    }

    //  class ColorBuffer private ( val width: Int, val height: Int, buf: ByteBuffer ) extends Image[Color] {
    //
    //    import ColorBuffer._
    //
    //    protected def create( width: Int, height: Int ): Image = ColorBuffer( width, height )
    //
    //    def getPosition( x: Int, y: Int ): Int = (x + y * width) * colorSize + headerSize
    //
    //    def position( x: Int, y: Int ): Unit = getPosition(x, y) |> buf.position
    //
    //    def getColor: Color = Color( buf.getFloat, buf.getFloat, buf.getFloat )
    //
    //    def apply( x: Int, y: Int ): Color = if( within(x, y) )  {
    //      position( x, y )
    //      getColor
    //    } else Color.Black
    //
    //    def update( c: Color ): Unit = {
    //      buf.putFloat( c.red )
    //      buf.putFloat( c.green )
    //      buf.putFloat( c.blue )
    //    }
    //
    //    def update( x: Int, y: Int, c: Color ): Unit = if( within(x, y) ) {
    //      position( x, y )
    //      update( c )
    //    }
    //
    //    def update( x: Int, y: Int, c: Int ): Unit = update( x, y, Color( c ) )
    //
    //    def update( f: Color => Color )( x: Int, y: Int ): Unit = if( within(x, y) ) {
    //      position( x, y )
    //      buf.mark
    //      val c = f( getColor )
    //      buf.reset
    //      update( c )
    //    }
    //
    //    def toBufferedImage: BufferedImage = {
    //      val bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB )
    //      position( 0, 0 )
    //      for( y <- 0 until height; x <- 0 until width ) bi.setRGB( x, y, getColor.toInt )
    //      bi
    //    }
    //
    //    def write( file: String ): Unit = {
    //      val ch = (new FileOutputStream( file )).getChannel
    //      buf.position( 0 )
    //      ch.write( buf )
    //      ch.close
    //    }
    //
    //    def write( format: String, file: String ): Unit = {
    //      ImageIO.write(toBufferedImage, format, new File(file))
    //    }
    //
    //  }

    //  object ColorBuffer {
    //
    //    private val headerSize = 8
    //    private val colorSize = 12
    //
    //    private def getBufferSize( width: Int, height: Int ): Int = headerSize + width * height * colorSize
    //
    //    private def createEmptyBuffer( width: Int, height: Int ): ByteBuffer = {
    //      val bufSize = getBufferSize( width, height )
    //      ByteBuffer.allocateDirect( bufSize )
    //    }
    //
    //    private def readSize( file: String ): (Int, Int) = {
    //      val tmp = ByteBuffer.allocateDirect( 8 )
    //      val is = new FileInputStream( file ).getChannel
    //      is.read( tmp )
    //      tmp.position( 0 )
    //      val width = tmp.getInt
    //      val height = tmp.getInt
    //      is.close
    //      ( width, height )
    //    }
    //
    //    def apply( width: Int, height: Int ): ColorBuffer = {
    //      val buf = createEmptyBuffer( width, height )
    //      buf.putInt( width )
    //      buf.putInt( height )
    //      new ColorBuffer( width, height, buf )
    //    }
    //
    //    def apply( file: String ): ColorBuffer = {
    //      val ( width, height ) = readSize( file )
    //      val input = new FileInputStream( file ).getChannel
    //      val buf = createEmptyBuffer( width, height )
    //      input.read( buf )
    //      input.close
    //      new ColorBuffer( width, height, buf )
    //    }
    //
    //  }


  }
}
