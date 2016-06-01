package net.ruffy.marble.graphics2d {

  trait GraphicsEnv {

    env: ImageEnv with ColorEnv =>

    abstract class Graphics {

      val image: Image[Color]

      def drawLine( x0: Int, y0: Int, x1: Int, y1: Int, argb: Int ): Unit

      def drawTriangle( x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int, argb: Int ): Unit

      def fillTriangle( x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int, argb: Int ): Unit

      def fillTriangle( x0: Int, y0: Int, c0: Int, x1: Int, y1: Int, c1: Int, x2: Int, y2: Int, c2: Int ) = Graphics.fillTriangle(
        image.update,
        x0, y0, c0,
        x1, y1, c1,
        x2, y2, c2
      )

      def fillTriangleZ(
        x0: Int, y0: Int, z0: Int, c0: Int,
        x1: Int, y1: Int, z1: Int, c1: Int,
        x2: Int, y2: Int, z2: Int, c2: Int
      ) = Graphics.fillTriangleZ(
        (x, y, z, c) => if( image.within( x, y ) && z < image.zbuffer(x, y) ) {
          image.update( x, y, c )
          image.zbuffer.update( x, y, z )
        },
        x0, y0, z0, c0,
        x1, y1, z1, c1,
        x2, y2, z2, c2
      )

    }

    object Graphics {

      def apply( _image: Image[Color] ) = new Graphics {

        val image = _image

        def drawLine( x0: Int, y0: Int, x1: Int, y1: Int, argb: Int ): Unit = Graphics.drawLine( image.update, x0, y0, x1, y1, argb )

        def drawTriangle( x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int, argb: Int ): Unit = {
          drawLine( x0, y0, x1, y1, argb )
          drawLine( x1, y1, x2, y2, argb )
          drawLine( x2, y2, x0, y0, argb )
        }

        def fillTriangle( x0: Int, y0: Int, x1: Int, y1: Int, x2: Int, y2: Int, argb: Int ): Unit = Graphics.fillTriangle(
          image.update, x0, y0, argb, x1, y1, argb, x2, y2, argb
        )

      }

      //def getRgb( rgb: Int ) = (rgb >> 16 & 0xff, rgb >> 8 & 0xff, rgb & 0xff)
      //def toRgb( r: Int, g: Int, b: Int ) = 0xff000000 | math.round(r) << 16 | math.round(g).toInt << 8 | math.round(b)

      def drawLine( pixelWriter: (Int, Int, Int) => Unit, x0: Int, y0: Int, x1: Int, y1: Int, argb: Int ) = {
        val dx = math.abs( x1 - x0 )
        val dy = math.abs( y1 - y0 )
        val sx = if( x0 < x1 ) 1 else -1
        val sy = if( y0 < y1 ) 1 else -1
        var e = dx - dy
        var k = true
        var x = x0
        var y = y0
        var cnt = 0
        while( k ) {
          cnt += 1
          pixelWriter(x, y, argb)
          if( x == x1 && y == y1 ) {
            k = false
          } else {
            val e2 = 2*e
            if( e2 > -dy ) {
              e = e - dy
              x = x + sx
            }
            if( e2 < dx ) {
              e = e + dx
              y = y + sy
            }
          }
        } // end while
          //println("dx: "+dx+", dy: "+dy+", "+cnt)
      }

      class PixelIncrementer( x0: Int, y0: Int, x1: Int, y1: Int ) {
        val dx = math.abs( x1 - x0 )
        val dy = math.abs( y1 - y0 )
        val sx = if( x0 < x1 ) 1 else -1
        val sy = if( y0 < y1 ) 1 else -1
        var e = dx - dy
        var k = true
        var x = x0
        var y = y0

        def increment = {
          val e2 = 2*e
          if( e2 > -dy ) {
            e = e - dy
            x = x + sx
          }
          if( e2 < dx ) {
            e = e + dx
            y = y + sy
          }
        }
      }

      /**
        * @param x0 開始の色
        * @param x1 最後の色
        * @param dy 斜辺の距離(ピクセル数)
        */
      class ColorIncrementer( x0: Int, x1: Int, dy: Int ) {

        val dx = math.abs( x1 - x0 )
        val dx2 = dx*2
        val dy2 = dy*2
        val sx = if( x0 < x1 ) 1 else -1

        var ex = dx
        var ey = dy
        var x = x0

        def increment = {
          while( ey <= ex ) {
            ey += dy2
            if( x != x1 ) x += sx
          }
          ex += dx2
        }

      }

      trait ZDepth extends LineDrawer {
        val z0: Int
        val z1: Int
        val dist: Int
        val izd = new ColorIncrementer( z0, z1, dist )

        val xyzcs = new Array[(Int, Int, Int)](dy + 1)

        def z = izd.x
        def xzc( y: Int ) = xyzcs( idxy( y ) )

        abstract override def increment = {
          super.increment
          izd.increment
        }
      }

      class LineDrawer(
        val x0: Int, val y0: Int, val c0: Int,
        val x1: Int, val y1: Int, val c1: Int
      ) {

        val (r0, g0, b0) = (c0 >> 16 & 0xff, c0 >> 8 & 0xff, c0 & 0xff)
        val (r1, g1, b1) = (c1 >> 16 & 0xff, c1 >> 8 & 0xff, c1 & 0xff)
        val dx = math.abs(x1 - x0)
        val dy = math.abs(y1 - y0)
        val dist = if( dx > dy ) dx else dy // 斜辺の距離
        val my = y0 min y1
        val ixy = new PixelIncrementer( x0, y0, x1, y1 )
        val ird = new ColorIncrementer( r0, r1, dist )
        val igd = new ColorIncrementer( g0, g1, dist )
        val ibd = new ColorIncrementer( b0, b1, dist )

        val xycs = new Array[(Int, Int)](dy + 1)

        def x = ixy.x
        def y = ixy.y
        def color = 0xff000000 | ird.x << 16 | igd.x << 8 | ibd.x
        def idxy( y: Int ) = y - my
        def xc( y: Int ) = xycs( idxy( y ) )
        def end = x == x1 && y == y1

        def increment = {
	      // x, yの両方の増加回数は、斜辺の距離と等しいので、色の増加回数と同じ回数だけループになる
          ixy.increment
          ird.increment
          igd.increment
          ibd.increment
        }

        def draw( pixelWriter: (Int, Int, Int) => Unit ): Unit = {
          var k = true
          while( k ) {
            //println( ln.x+", "+ln.y+", "+ln.px+", "+ln.py+", "+ex+", "+ey )
            val c = color
            xycs( idxy( y ) ) = (x, c)
            pixelWriter( x, y, c )
            if( end ) k = false
            else increment
          }
        }
      }

      /**
        * A class to draw line with z buffer.
        */
      class LineDrawerZ(
        x0: Int, y0: Int, val z0: Int, c0: Int,
        x1: Int, y1: Int, val z1: Int, c1: Int
      ) extends LineDrawer(x0, y0, c0, x1, y1, c1) with ZDepth {

        def draw( pixelWriter: (Int, Int, Int, Int) => Unit ): Unit = {
          var k = true
          while( k ) {
            //println( ln.x+", "+ln.y+", "+ln.px+", "+ln.py+", "+ex+", "+ey )
            val c = color
            xyzcs( idxy( y ) ) = (x, z, c)
            pixelWriter( x, y, z, c )
            if( end ) k = false
            else increment
          }
        }

      }

      def fillTriangle( pixelWriter: (Int, Int, Int) => Unit, x0: Int, y0: Int, c0: Int, x1: Int, y1: Int, c1: Int, x2: Int, y2: Int, c2: Int ) = {
        if( y0 <= y1 && y1 <= y2 )      fillSortedTriangle( pixelWriter, x0, y0, c0, x1, y1, c1, x2, y2, c2 )
        else if( y0 <= y2 && y2 <= y1 ) fillSortedTriangle( pixelWriter, x0, y0, c0, x2, y2, c2, x1, y1, c1 )
        else if( y1 <= y0 && y0 <= y2 ) fillSortedTriangle( pixelWriter, x1, y1, c1, x0, y0, c0, x2, y2, c2 )
        else if( y1 <= y2 && y2 <= y0 ) fillSortedTriangle( pixelWriter, x1, y1, c1, x2, y2, c2, x0, y0, c0 )
        else if( y2 <= y0 && y0 <= y1 ) fillSortedTriangle( pixelWriter, x2, y2, c2, x0, y0, c0, x1, y1, c1 )
        else if( y2 <= y1 && y1 <= y0 ) fillSortedTriangle( pixelWriter, x2, y2, c2, x1, y1, c1, x0, y0, c0 )
      }

      def fillTriangleZ(
        pixelWriter: (Int, Int, Int, Int) => Unit,
        x0: Int, y0: Int, z0: Int, c0: Int,
        x1: Int, y1: Int, z1: Int, c1: Int,
        x2: Int, y2: Int, z2: Int, c2: Int
      ) = {
        if( y0 <= y1 && y1 <= y2 )      fillSortedTriangleZ( pixelWriter, x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2 )
        else if( y0 <= y2 && y2 <= y1 ) fillSortedTriangleZ( pixelWriter, x0, y0, z0, c0, x2, y2, z2, c2, x1, y1, z1, c1 )
        else if( y1 <= y0 && y0 <= y2 ) fillSortedTriangleZ( pixelWriter, x1, y1, z1, c1, x0, y0, z0, c0, x2, y2, z2, c2 )
        else if( y1 <= y2 && y2 <= y0 ) fillSortedTriangleZ( pixelWriter, x1, y1, z1, c1, x2, y2, z2, c2, x0, y0, z0, c0 )
        else if( y2 <= y0 && y0 <= y1 ) fillSortedTriangleZ( pixelWriter, x2, y2, z2, c2, x0, y0, z0, c0, x1, y1, z1, c1 )
        else if( y2 <= y1 && y1 <= y0 ) fillSortedTriangleZ( pixelWriter, x2, y2, z2, c2, x1, y1, z1, c1, x0, y0, z0, c0 )
      }

      def fillSortedTriangle( pixelWriter: (Int, Int, Int) => Unit, x0: Int, y0: Int, c0: Int, x1: Int, y1: Int, c1: Int, x2: Int, y2: Int, c2: Int ) = {

        val l01 = new LineDrawer( x0, y0, c0, x1, y1, c1 )
        val l02 = new LineDrawer( x0, y0, c0, x2, y2, c2 )
        val l12 = new LineDrawer( x1, y1, c1, x2, y2, c2 )

        l01.draw( pixelWriter )
        l02.draw( pixelWriter )
        l12.draw( pixelWriter )

        for( y <- y0 to y1 ) {
          val (x0, c0) = l01.xc( y )
          val (x1, c1) = l02.xc( y )
          val ln = new LineDrawer( x0, y, c0, x1, y, c1 )
          ln.draw( pixelWriter )
        }

        for( y <- y1 to y2 ) {
          val (x0, c0) = l02.xc( y )
          val (x1, c1) = l12.xc( y )
          val ln = new LineDrawer( x0, y, c0, x1, y, c1 )
          ln.draw( pixelWriter )
        }


      }

      def fillSortedTriangleZ(
        pixelWriter: (Int, Int, Int, Int) => Unit,
        x0: Int, y0: Int, z0: Int, c0: Int,
        x1: Int, y1: Int, z1: Int, c1: Int,
        x2: Int, y2: Int, z2: Int, c2: Int
      ) = {

        val l01 = new LineDrawerZ( x0, y0, z0, c0, x1, y1, z1, c1 )
        val l02 = new LineDrawerZ( x0, y0, z0, c0, x2, y2, z2, c2 )
        val l12 = new LineDrawerZ( x1, y1, z1, c1, x2, y2, z2, c2 )

        l01.draw( pixelWriter )
        l02.draw( pixelWriter )
        l12.draw( pixelWriter )

        for( y <- y0 to y1 ) {
          val (x0, z0, c0) = l01.xzc( y )
          val (x1, z1, c1) = l02.xzc( y )
          val ln = new LineDrawerZ( x0, y, z0, c0, x1, y, z1, c1 )
          ln.draw( pixelWriter )
        }

        for( y <- y1 to y2 ) {
          val (x0, z0, c0) = l02.xzc( y )
          val (x1, z1, c1) = l12.xzc( y )
          val ln = new LineDrawerZ( x0, y, z0, c0, x1, y, z1, c1 )
          ln.draw( pixelWriter )
        }


      }

      def fillRectangleZ(
        pixelWriter: (Int, Int, Int, Int) => Unit,
        x0: Int, y0: Int, z0: Int, c0: Int,
        x1: Int, y1: Int, z1: Int, c1: Int,
        x2: Int, y2: Int, z2: Int, c2: Int,
        x3: Int, y3: Int, z3: Int, c3: Int
      ) = {
        fillTriangleZ(
          pixelWriter,
          x0, y0, z0, c0,
          x1, y1, z1, c1,
          x2, y2, z2, c2
        )
        fillTriangleZ(
          pixelWriter,
          x2, y2, z2, c2,
          x3, y3, z3, c3,
          x0, y0, z0, c0
        )
      }

      def main( args: Array[String] ) {
        fillSortedTriangle( (x, y, c) => (), 5, 5, 0xffff0000, 50, 10, 0xff0000ff, 25, 20, 0xff00ff00 )
      }

    }

  }
}
