package net.ruffy.marble.math {

  trait NumberEnv {

    import scala.reflect.ClassTag
    import scala.{ Double => SDouble, Float => SFloat, Int => SInt }
    import java.nio.{ ByteBuffer }

    type NumType
    type Num <: Number[NumType, Num]

    implicit val numTag: ClassTag[Num]
    val numSize: Int

    protected implicit def mkNum( x: SDouble ): Num
    protected implicit def mkNum( x: SFloat ): Num
    protected implicit def mkNum( x: SInt ): Num

    protected def getNumByteBuf( buf: ByteBuffer ): Num
    protected def putNumByteBuf( buf: ByteBuffer, v: Num ): Unit

  }

}
