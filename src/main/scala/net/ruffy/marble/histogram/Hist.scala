package net.ruffy.marble.histogram {

  import net.ruffy.util.{ Result, Good, Bad }

  abstract class Hist {

  }

  class Axis( val nbins: Int, val min: Double, val max: Double ) {

    lazy val binWidth = (max - min)/nbins

    def findBin( x: Double ): Result[Int, Axis.OutOfRange] = {
      val bin = (( x - min ) / binWidth).toInt
      if( bin < 0 ) Bad(Axis.UnderFlow)
      else if( bin >= nbins ) Bad(Axis.OverFlow)
      else Good(bin)
    }

    def binMin( i: Int ): Double = i * binWidth
    def binMax( i: Int ): Double = (i + 1) * binWidth
    def binCenter( i: Int): Double = i * binWidth + binWidth/2

  }

  object Axis {

    abstract class OutOfRange
    object UnderFlow extends OutOfRange
    object OverFlow extends OutOfRange

  }

  class Hist1D( nbins: Int, min: Double, max: Double ) extends Hist {

    private lazy val bins = new Array[Double]( nbins )

    private var maxCont: Double = 0
    private var minCont: Double = 0

    lazy val axis = new Axis( nbins, min, max )

    def fill( x: Double ): Unit = {
      val Good(bin) = axis.findBin( x )
      val cont = bins( bin )
      val res = if( cont == 0 ) 1 else cont + 1
      if( res > maxCont ) maxCont = res
      bins( bin ) = res
    }

    override def toString = {
      bins.zipWithIndex.map { case ( c, i ) => "| %4d | %3f - %3f | %6.0f |".format(i, axis.binMin( i ), axis.binMax( i ), c) }.mkString("\n")
    }

  }

  class Hist2D(
    nbinsx: Int, xmin: Double, xmax: Double,
    nbinsy: Int, ymin: Double, ymax: Double
  ) extends Hist {

    private lazy val bins = new Array[Array[Double]]( nbinsy )
    for( y <- 0 until nbinsy ) bins( y ) = new Array( nbinsx )

    private var maxCont: Double = 0
    private var minCont: Double = 0

    lazy val xaxis = new Axis( nbinsx, xmin, xmax )
    lazy val yaxis = new Axis( nbinsy, ymin, ymax )

    def fill( x: Double, y: Double ): Unit = {
      val Good(xbin) = xaxis.findBin( x )
      val Good(ybin) = yaxis.findBin( y )
      val cont = bins( ybin )( xbin )
      val res = if( cont == 0 ) 1 else cont + 1
      if( res > maxCont ) maxCont = res
      bins( ybin )( xbin ) = res
    }

    def fill( n: (Double, Double) ): Unit = fill( n._1, n._2 )

    override def toString = {
      "|      |%s\n".format( bins.head.zipWithIndex.map { case ( c, x ) => "%6d".format( x ) }.mkString("| ", " | ", " |")) +
      bins.zipWithIndex.map { case ( xs, y ) =>
        "| %4d |%s".format( y, xs.zipWithIndex.map { case ( c, x ) => "%6.0f".format( c ) }.mkString("| ", " | ", " |") )
      }.mkString("\n")
    }


  }

}
