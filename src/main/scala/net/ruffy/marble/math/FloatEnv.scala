package net.ruffy.marble.math {

  import scala.reflect.ClassTag
  import scala.{ Double => SDouble, Float => SFloat, Int => SInt }
  import java.nio.{ ByteBuffer }

  trait FloatEnv extends NumberEnv {

    type NumType = SFloat
    type Num = Float

    implicit val numTag: ClassTag[Num] = ClassTag( classOf[Float] )
    val numSize: Int = 4

    protected implicit def mkNum( x: SDouble ) = new Float( x.toFloat )
    protected implicit def mkNum( x: SFloat ) = new Float( x )
    protected implicit def mkNum( x: SInt ) = new Float( x.toFloat )

    protected def getNumByteBuf( buf: ByteBuffer ) = mkNum( buf.getFloat )
    protected def putNumByteBuf( buf: ByteBuffer, v: Float ) = buf.putFloat( v.underlying )

  }

}
