package net.ruffy.marble.math {

  import scala.reflect.ClassTag
  import scala.{ Double => SDouble, Float => SFloat, Int => SInt }
  import java.nio.{ ByteBuffer }

  trait DoubleEnv extends NumberEnv {

    type NumType = SDouble
    type Num = Double

    implicit val numTag: ClassTag[Num] = ClassTag( classOf[Double] )
    val numSize: Int = 8

    protected implicit def mkNum( x: SDouble ) = new Double( x )
    protected implicit def mkNum( x: SFloat ) = new Double( x.toDouble )
    protected implicit def mkNum( x: SInt ) = new Double( x.toDouble )

    protected def getNumByteBuf( buf: ByteBuffer ) = mkNum( buf.getDouble )
    protected def putNumByteBuf( buf: ByteBuffer, v: Double ) = buf.putDouble( v.underlying )

  }

}
