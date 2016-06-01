package net.ruffy.marble.graphics3d {

  import net.ruffy.marble.math.{ Vector3 }
  import net.ruffy.marble.graphics2d.{ Image, Color }
  import shapes.{ BoundingSphere }
  import java.nio.{ ByteBuffer }
  import java.io.{ FileOutputStream, FileInputStream }
  import scala.reflect.ClassTag

  abstract class Octree[B, C <: Octree[B, C]](
    val xmin: Float, val xmax: Float,
    val ymin: Float, val ymax: Float,
    val zmin: Float, val zmax: Float,
    val capacity: Int,
    val parent: Option[C]
  ) {

    self: C =>

    import Octree._

    type DataType = (Vector3, B)

    private var _data: List[DataType] = Nil
    private var ndata = 0
    private var _children: Option[Array[C]] = None

    lazy val xcenter: Float = (xmin + xmax)/2
    lazy val ycenter: Float = (ymin + ymax)/2
    lazy val zcenter: Float = (zmin + zmax)/2
    lazy val center = Vector3( xcenter, ycenter, zcenter )
    lazy val boundingSphere = BoundingSphere(center, (center -> Vector3( xmin, ymin, zmin )).abs )

    def data = _data
    def children = _children

    protected def create(
      xmin: Float, xmax: Float,
      ymin: Float, ymax: Float,
      zmin: Float, zmax: Float,
      capacity: Int,
      parent: Option[C]
    ): C

    /**
      * 子を作成する。
      * 既に作られている場合には、何もしない。
      */
    private def createChildren(implicit tag: ClassTag[C]) = _children match {
      case None =>
        _children = Some(Array[C](
          create( xmin, xcenter, ymin, ycenter, zmin, zcenter, capacity, Some( this ) ), // 0: (0, 0, 0)
          create( xcenter, xmax, ymin, ycenter, zmin, zcenter, capacity, Some( this ) ), // 1: (1, 0, 0)
          create( xmin, xcenter, ycenter, ymax, zmin, zcenter, capacity, Some( this ) ), // 2: (0, 1, 0)
          create( xcenter, xmax, ycenter, ymax, zmin, zcenter, capacity, Some( this ) ), // 3: (1, 1, 0)

          create( xmin, xcenter, ymin, ycenter, zcenter, zmax, capacity, Some( this ) ), // 4: (0, 0, 1)
          create( xcenter, xmax, ymin, ycenter, zcenter, zmax, capacity, Some( this ) ), // 5: (1, 0, 1)
          create( xmin, xcenter, ycenter, ymax, zcenter, zmax, capacity, Some( this ) ), // 6: (0, 1, 1)
          create( xcenter, xmax, ycenter, ymax, zcenter, zmax, capacity, Some( this ) )  // 7: (1, 1, 1)
        ) )

      case _ => ()
    }

    private def xindex( a: Vector3 ): Option[Int] =
      if( a.x >= xmin && a.x < xcenter ) Some(0)
      else if( a.x >= xcenter && a.x < xmax ) Some(1)
      else None

    private def yindex( a: Vector3 ): Option[Int] =
      if( a.y >= ymin && a.y < ycenter ) Some(0)
      else if( a.y >= ycenter && a.y < ymax ) Some(1)
      else None

    private def zindex( a: Vector3 ): Option[Int] =
      if( a.z >= zmin && a.z < zcenter ) Some(0)
      else if( a.z >= zcenter && a.z < zmax ) Some(1)
      else None

    def withinx( x: Float ): Boolean = x >= xmin && x < xmax
    def withiny( y: Float ): Boolean = y >= ymin && y < ymax
    def withinz( z: Float ): Boolean = z >= zmin && z < zmax

    def contains( a: Vector3 ): Boolean = withinx( a.x ) && withiny( a.y ) && withinz( a.z )

    def index( xidx: Int, yidx: Int, zidx: Int ): Int = {
      if( xidx < 0 || xidx > 1 ) throw new Exception("Out of range in x index: %d".format( xidx ))
      if( yidx < 0 || yidx > 1 ) throw new Exception("Out of range in y index: %d".format( xidx ))
      if( zidx < 0 || zidx > 1 ) throw new Exception("Out of range in z index: %d".format( xidx ))
      xidx + 2*yidx + 4*zidx
    }

    /**
      * 点aを含む子のインデックスを取得する。
      */
    def index( a: Vector3 ): Option[Int] = (xindex( a ), yindex( a ), zindex( a )) match {
      case (Some(x), Some(y), Some(z)) => Some( index( x, y, z ) )
      case _ => None
    }

    def getChild( idx: Int ): Option[C] = _children match {
      case Some( c ) => Some( c( idx ) )
      case None => None
    }

    /**
      * 点aを含む子を探索する。
      */
    def findChild( a: Vector3 ): Option[C] = _children match {
      case None => None
      case Some( c ) => index( a ) match {
        case Some( i ) => Some( c( i ) )
        case None => None
      }
    }

    protected def onUpdate( b: B ): Unit = {}

    /**
      * このOctreeの容量を超えてなければ、データを追加する。
      * 容量を超えていれば、このOctreeを８分割して子を作り、
      * 子のOctreeに追加する。
      */
    def add( a: Vector3, b: B )(implicit tag: ClassTag[C]): Unit = if( ndata < capacity ) {
      _data = (a, b) :: _data
      ndata += 1
      onUpdate( b )
    } else {
      createChildren // 既に子を持っている場合は子を作成しない
      // このOctreeが持っているデータを子に移す
      _data.foreach { case (p, q) => findChild( p ).foreach( _.add( p, q ) ) }
      findChild( a ).foreach( _.add( a, b ) )
      _data = Nil
    }

    /**
      * 点aを含む最小のOctreeを探索する。
      */
    def find( a: Vector3 ): Option[C] = findChild( a ) match {
      case None => if( contains( a ) ) Some( this ) else None
      case Some( c ) => c find a 
    }

    def size: Int = _children match {
      case None => ndata
      case Some( c ) => c.foldLeft( 0 )( (a, b) => a + b.size )
    }

    def foreach( f: (Vector3, B) => Unit ): Unit = _children match {
      case None => _data.foreach { case (a, b) => f( a, b )  }
      case Some( c ) => c.foreach( _ foreach f )
    }

    /**
      * このインスタンス以下のOctreeのうち、bに含まれている最大のOctreeを選別する。
      * bに含まれているOctreeがない場合には、bを含む最小のOctreeを選別する。
      */
    def filterChildren( b: BoundingSphere, f: (Int, C) => Unit ): Unit = boundingSphere.contains( b ) match {
      // 離れている場合
      case 3 => ()
      // bがこのOctreeを含んでいる場合
      case 2 => f( 2, this )
      // このOctreeがbを含んでいるか接している場合
      case n => _children match {
        case None => f( n, this )
        case Some( cs ) => cs.foreach { _.filterChildren( b, f ) }
      }
    }

    def write( fname: String, elementSize: Int, toBytes: (ByteBuffer, B) => Unit ): Unit = {
      val datasize = size
      val filesize = HeaderSize + datasize * (KeySize + elementSize)

      val buf = ByteBuffer.allocateDirect( filesize )
      buf.position( 0 )
      buf.putInt( filesize )
      buf.putInt( elementSize )
      buf.putFloat( xmin )
      buf.putFloat( xmax )
      buf.putFloat( ymin )
      buf.putFloat( ymax )
      buf.putFloat( zmin )
      buf.putFloat( zmax )
      buf.putInt( capacity )
      buf.putInt( datasize )

      foreach { (k, v) =>
        buf.putFloat( k.x )
        buf.putFloat( k.y )
        buf.putFloat( k.z )
        toBytes( buf, v )
      }

      val ch = (new FileOutputStream( fname )).getChannel
      buf.position( 0 )
      ch.write( buf )
      ch.close
    }

    override def toString = "(%.3f, %.3f, %.3f, %.3f, %.3f, %.3f)".format( xmin, xmax, ymin, ymax, zmin, zmax )

  }

  object Octree {

    class DefaultOctree[B](
      xmin: Float, xmax: Float,
      ymin: Float, ymax: Float,
      zmin: Float, zmax: Float,
      capacity: Int,
      parent: Option[DefaultOctree[B]]
    ) extends Octree[B, DefaultOctree[B]]( xmin, xmax, ymin, ymax, zmin, zmax, capacity, parent ) {

      protected def create(
        xmin: Float, xmax: Float,
        ymin: Float, ymax: Float,
        zmin: Float, zmax: Float,
        capacity: Int,
        parent: Option[DefaultOctree[B]]
      ): DefaultOctree[B] = new DefaultOctree[B]( xmin, xmax, ymin, ymax, zmin, zmax, capacity, parent )

    }

    lazy val HeaderSize = 40 // filesize: 4 + element size: 4 + constructor:28 + data size: 4
    lazy val KeySize = 12 // x: 4 + y: 4 + z: 4

    def apply[B]( 
      xmin: Float, xmax: Float,
      ymin: Float, ymax: Float,
      zmin: Float, zmax: Float,
      capacity: Int
    ): DefaultOctree[B] = new DefaultOctree[B]( xmin, xmax, ymin, ymax, zmin, zmax, capacity, None )

    def apply[B]( 
      xmin: Float, xmax: Float,
      ymin: Float, ymax: Float,
      zmin: Float, zmax: Float
    ): DefaultOctree[B] = Octree[B]( xmin, xmax, ymin, ymax, zmin, zmax, 20 )

    def apply[B]( sphere: BoundingSphere ): DefaultOctree[B] = Octree[B](
      sphere.center.x - sphere.radius, sphere.center.x + sphere.radius,
      sphere.center.y - sphere.radius, sphere.center.y + sphere.radius,
      sphere.center.z - sphere.radius, sphere.center.z + sphere.radius
    )

    def apply[B, C <: Octree[B, C]](
      fname: String,
      fromBytes: ByteBuffer => B,
      create: (Float, Float, Float, Float, Float, Float, Int) => C
    )(implicit tag: ClassTag[C]): C = {
      val tmp = ByteBuffer.allocateDirect( HeaderSize )
      val is = new FileInputStream( fname ).getChannel
      is.read( tmp )
      tmp.position( 0 )
      val filesize = tmp.getInt
      val elemSize = tmp.getInt
      val xmin = tmp.getFloat
      val xmax = tmp.getFloat
      val ymin = tmp.getFloat
      val ymax = tmp.getFloat
      val zmin = tmp.getFloat
      val zmax = tmp.getFloat
      val capacity = tmp.getInt
      val ndata = tmp.getInt
      val self = create( xmin, xmax, ymin, ymax, zmin, zmax, capacity )
      is.close

      val input = new FileInputStream( fname ).getChannel
      val buf = ByteBuffer.allocateDirect( filesize )
      input.read( buf )
      input.close
      buf.position( HeaderSize )

      for( i <- 0 until ndata ) {
        val x = buf.getFloat
        val y = buf.getFloat
        val z = buf.getFloat
        self.add( Vector3( x, y, z ), fromBytes( buf ) )
      }

      self
    }

    def apply[B](
      fname: String,
      fromBytes: ByteBuffer => B
    ): DefaultOctree[B] = Octree[B, DefaultOctree[B]](
      fname,
      fromBytes,
      (x0, x1, y0, y1, z0, z1, cap) => Octree[B](x0, x1, y0, y1, z0, z1, cap)
    )
  }

}
