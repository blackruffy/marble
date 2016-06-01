package net.ruffy.marble.graphics3d {

  import net.ruffy.marble.math.{ Math, Vector3 }
  import net.ruffy.marble.graphics3d.shapes.{ BoundingSphere }
  import net.ruffy.marble.graphics2d.{ Color, Image }
  import java.nio.{ ByteBuffer }
  import java.io.{ FileInputStream, FileOutputStream }

  /**
    * フォトンマップの実装。
    * 空間を格子状に分割しており、３次元のヒストグラムのようなもの。
    * 各ビンには、Vector型とColor型のタプルのリストが保存されている。
    */
  class Grid(
    nbinsx: Int, xmin: Float, xmax: Float,
    nbinsy: Int, ymin: Float, ymax: Float,
    nbinsz: Int, zmin: Float, zmax: Float
  ) {
    import Grid.Bin

    lazy val nbinsxy: Int = nbinsx * nbinsy
    lazy val nbins: Int = nbinsxy * nbinsz
    lazy val binWidthX: Float = (xmax - xmin) / nbinsx
    lazy val binWidthY: Float = (ymax - ymin) / nbinsy
    lazy val binWidthZ: Float = (zmax - zmin) / nbinsz

    lazy val bins = new Array[Grid.Bin]( nbins )

    def findBinIndex( p: Vector3 ): Int = {
      val ix = ((p.x - xmin)/binWidthX).toInt
      val iy = ((p.y - ymin)/binWidthY).toInt
      val iz = ((p.z - zmin)/binWidthZ).toInt
      ix + nbinsx * iy + nbinsxy * iz
    }

    /** indexからビンを取得する。*/
    def getBin( idx: Int ): Option[Bin] = bins( idx ) match {
      case null => None
      case bin => Some(bin)
    }

    /** 位置からビンを取得する。*/
    def getBin( p: Vector3 ): Option[Bin] = getBin( findBinIndex( p ) )

    /** ビンにデータを追加する。*/
    def update( p: Grid.BinType ): Unit = update( findBinIndex( p._1 ), p )

    def update( idx: Int, p: Grid.BinType ): Unit = if( idx < nbins ) {
      val bin = bins( idx ) match {
        case null =>
          val bin = new Bin
          bins( idx ) = bin
          bin
        case bin => bin
      }
      bin.update( p )
    }

    def write( fname: String ): Unit = {
      // データ: binのインデックス,binのサイズ,float,float,floatがnbins回繰り返す
      val bufsize = Grid.HeaderSize + bins.foldLeft( 0 ) {
        case (a, null) => a + 4 + 4
        case (a, b) => a + 4 + 4 + b.size * Grid.BinTypeSize
      }
      val buf = ByteBuffer.allocateDirect( bufsize )
      buf.position( 0 )
      buf.putInt( bufsize )
      buf.putInt( nbinsx )
      buf.putFloat( xmin )
      buf.putFloat( xmax )
      buf.putInt( nbinsy )
      buf.putFloat( ymin )
      buf.putFloat( ymax )
      buf.putInt( nbinsz )
      buf.putFloat( zmin )
      buf.putFloat( zmax )

      for( i <- 0 until nbins ) {
        val bin = bins( i )
        if( bin == null ) {
          buf.putInt( i ) // index
          buf.putInt( 0 ) // bin size
        } else {
          buf.putInt( i ) // index
          buf.putInt( bin.size ) // bin size
          bin.points.foreach { case (p, c) =>
            buf.putFloat( p.x )
            buf.putFloat( p.y )
            buf.putFloat( p.z )
            buf.putFloat( c.alpha )
            buf.putFloat( c.red )
            buf.putFloat( c.green )
            buf.putFloat( c.blue )
          }
        }
      }

      val ch = (new FileOutputStream( fname )).getChannel
      buf.position( 0 )
      ch.write( buf )
      ch.close
    }

    def write( toScreen: Vector3 => (Int, Int, Int), image: Image[Color], ext: String, fname: String ): Unit = {
      bins.foreach { b => if( b != null ) {
        b.points.foreach { case (p, c) =>
          val (x, y, z) = toScreen( p )
          image.update( x, y, c )
        }
      } }

      image.write( ext, fname )
    }

  }

  object Grid {

    type BinType = (Vector3, Color)

    /** ヘッダーサイズ：40byte(ファイルサイズとコンストラクタの引数分) */
    val HeaderSize = 40

    val BinTypeSize = 28

    class Bin {

      private var _size: Int = 0
      private var ps: List[BinType] = Nil

      def size = _size

      def points = ps

      def update( p: BinType ): Unit = {
        ps = p :: ps
        _size += 1
      }

    }

    def apply( sphere: BoundingSphere, nbins: Int ): Grid = new Grid(
      nbins, sphere.center.x - sphere.radius, sphere.center.x + sphere.radius,
      nbins, sphere.center.y - sphere.radius, sphere.center.y + sphere.radius,
      nbins, sphere.center.z - sphere.radius, sphere.center.z + sphere.radius
    )

    def apply( fname: String ): Grid = {
      val tmp = ByteBuffer.allocateDirect( HeaderSize )
      val is = new FileInputStream( fname ).getChannel
      is.read( tmp )
      tmp.position( 0 )
      val bufsize = tmp.getInt
      val map = new Grid(
        tmp.getInt, tmp.getFloat, tmp.getFloat,
        tmp.getInt, tmp.getFloat, tmp.getFloat,
        tmp.getInt, tmp.getFloat, tmp.getFloat
      )
      is.close

      val input = new FileInputStream( fname ).getChannel
      val buf = ByteBuffer.allocateDirect( bufsize )
      input.read( buf )
      input.close
      buf.position( HeaderSize )
      for( i <- 0 until map.nbins ) {
        val idx = buf.getInt
        val binsize = buf.getInt
        for( j <- 0 until binsize ) {
          map( i ) = (
            Vector3( buf.getFloat, buf.getFloat, buf.getFloat ),
            Color( buf.getFloat, buf.getFloat, buf.getFloat, buf.getFloat )
          )
        }
      }

      map
    }

  }

}
