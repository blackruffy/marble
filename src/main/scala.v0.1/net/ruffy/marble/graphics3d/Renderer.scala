package net.ruffy.marble.graphics3d {

  import net.ruffy.marble.math.{ Vector3 }
  import net.ruffy.marble.graphics2d.{ Graphics, ZBuffer, Color }
  import net.ruffy.marble.graphics3d.shapes.{ Light, BoundingSphere }
  import Material.{ Medium, Emittance, Refractive }
  import net.ruffy.util.{ Result, Good, Bad, Empty, Implicits }
  import Implicits._

  trait Renderer {
    def render: Unit
  }

  object Renderer {

    /**
      * 衝突点の媒質を取得する
      */
    def medium( i: Intersection, croot: RenderableTree ) = if( i.inside_? ) croot.medium( i.point, List(i.shape) ) else i.shape.material.medium
  }

  trait WireFramer extends Renderer {
    val camera: Camera
    val root: RenderableTree
    val color: Int
    val graphics: Graphics

    lazy val croot = root toCamera camera

    def render: Unit = root.foreach( _.drawWireFrame( graphics, camera, color ) )

  }

  trait GouraudShader extends Renderer {
    val camera: Camera
    val root: RenderableTree
    val zbuf: ZBuffer
    val graphics: Graphics

    lazy val croot = root toCamera camera
    lazy val lights = croot.extractLights

    def render: Unit = croot.foreach( _.fillWithGouraudShading( graphics, camera, lights, zbuf ) )

  }

  trait RayTracer extends Renderer {

    val camera: Camera
    val root: RenderableTree
    val numberOfTimesToTrace: Int

    lazy val croot = root toCamera camera
    lazy val lights = croot.extractLights

    def shadow_?( light: Light, point: Vector3 ): Boolean = croot.nearestIntersection( Photon( point, (point -> light.center).normalize, Color.White, Medium.Default ) ) match {
      case Some( x ) => !x.shape.material.surface.isLight
      case _ => false
    }

    /**
      * @see RayTracer#trace(RenderableTree, Photon, List[Light], Int, Int, Intersection => Color): Color
      */
    def trace( ray: Photon, cnt: Int, difcol: Intersection => Color ): Color = RayTracer.trace( croot, ray, cnt, numberOfTimesToTrace, difcol )

    def trace( ray: Photon, difcol: Intersection => Color ): Color = trace( ray, 0 )

    /**
      * デフォルトの拡散面の色の計算方法を使用している。
      */
    def trace( ray: Photon, cnt: Int ): Color = trace( ray, cnt, { i =>
      lights.foldLeft[Color]( Color.Black ) { (c, l) =>
        c + ( if( shadow_?( l, i.point ) ) Color.Black else l.color( i.normal, i.point, i.shape.material, Material.Diffusivity ) )
      }
    })

    def trace( ray: Photon ): Color = trace( ray, 0 )

    def render( x0: Int, y0: Int, x1: Int, y1: Int ): Unit = for( y <- y0 until y1; x <- x0 until x1  ) {
      if( x == 0 ) println( "%d, %d".format( x, y ) )
      camera.screen.image.update( x, y, trace( camera.createPhoton( x, y ) ) )
    }

    def render: Unit = render( 0, 0, camera.screen.width, camera.screen.height )

  }

  object RayTracer {

    /**
      * 拡散面に到達するまでレイが分岐しながらトレースする。拡散面では光源から寄与を計算する。
      * @param difcol 拡散面の色を計算する関数
      */
    def trace( croot: RenderableTree, ray: Photon, cnt: Int, numberOfTimesToTrace: Int, difcol: Intersection => Color ): Color = {
      if( cnt < numberOfTimesToTrace ) croot.nearestIntersection( ray ).map { i =>
        val (diffIdx, reflIdx, refrIdx, emitIdx) = i.shape.material.surface.rates
        val cl = if( reflIdx > 0 ) trace( croot, i.createReflection, cnt + 1, numberOfTimesToTrace, difcol ) else Color.Black
        val cr = if( refrIdx > 0 ) trace( croot, i.createRefraction( Renderer.medium(i, croot) ), cnt +1, numberOfTimesToTrace, difcol ) else Color.Black
        val cd = if( diffIdx > 0 ) difcol( i ) else Color.Black
        val ce = if( emitIdx > 0 ) i.shape.material.surface.emissionColor else Color.Black
        //println("%d: refl:%s, refr:%s, diff:%s, emit:%s, %f, %f, %f, %f".format(cnt, cl, cr, cd, ce, reflIdx, refrIdx, diffIdx, emitIdx))
        cl * reflIdx + cr * refrIdx + cd * diffIdx + ce * emitIdx * 10
      }.getOrElse( Color.Black ) else Color.Black
    }

  }

  trait PathTracer extends Renderer {

    val camera: Camera
    val root: RenderableTree

    /**
      * フォトンが反射・屈折する回数
      */
    val numberOfTimesToTrace: Int

    lazy val croot = root toCamera camera
    lazy val lights = croot.extractLights

    /**
      * 拡散面・光源と衝突するまでフォトンを追跡する。
      * @return 衝突面でランダムに選ばれた材質の種類と衝突面。
      */
    def shootPhoton( ray: Photon, cnt: Int = 0 ): Option[(Material.Type, Intersection)] = if( cnt < numberOfTimesToTrace ) {
      croot.nearestIntersection( ray ).flatMap { i => i.shape.material.surface.random match { // 材質の選択
        case Material.Diffusivity => Some(Material.Diffusivity, i)
        case Material.Reflectance => shootPhoton( i.createReflection, cnt + 1 )
        case Material.Refractive => shootPhoton( i.createRefraction( Renderer.medium(i, croot) ), cnt + 1 )
        case Material.Emittance => Some( Material.Emittance, i )
      } }
    } else None

    /**
      * @see PathTracer#shootPhotonRecursively
      */
    def shootPhotonRecursively[A]( a: A )( func: (A, Material.Type, Intersection) => Result[A, A] , ray: Photon ): A =  PathTracer.shootPhotonRecursively( a )( croot, func, ray )

    //def shootPhotonRecursively( ray: Photon, num: Int, color: (Material.Type, Intersection) => Color ): Color = {
	//  shootPhotonRecursively[(Color, Material.Type, Int)]((Color.Black, Absorptance, 0))( {
    //    case ((c, _, cnt), t:Material.Emittance.type, i) => ((i.material.color, t, cnt + 1), cnt < num)
    //    case ((c, _, cnt), t, i) => ((if( cnt == 0 ) color( t, i ) else c + color( t, i ) * Math.pow(0.5f, cnt), t, cnt + 1), cnt < num)
	//  }, ray ) |> { case (c, _, _ ) => c }
    //}

    def diffuseColor( i: Intersection ): Color = PathTracer.normalizedColor( i.shape.material.surface.diffuseColor )

    def extractColors( ray: Photon ): List[Color] = ray.parent match {
      case None => ray.color :: Nil
      case Some( p ) => ray.color :: extractColors( p )
    }

    def shootPhotonRecursively( ray: Photon ): List[(Material.Type, Intersection, Int)] = {
	  shootPhotonRecursively[List[(Material.Type, Intersection, Int)]]( Nil )( {
        case ( Nil, t, i ) => Result( (t, i, 1) :: Nil, 1 < numberOfTimesToTrace )
        case ( xs@((_, _, cnt) :: _), t, i ) => Result( (t, i, cnt + 1) :: xs, cnt + 1 < numberOfTimesToTrace )
	  }, ray )
    }

    def render: Unit = for( i <- 1 to 10 ) {
      camera.screen.image.update { (x, y, c) =>
          shootPhoton( camera.createPhoton( x, y ) ) match {
            case Some( ( Emittance, isec ) ) => c + Color( 0x111111 )
            case _ => c
          }
      }
      camera.screen.image.write("png", "images/pathtrace%d.png".format( i ) )
    }

  }

  object PathTracer {

    def normalizedColor( c: Color ): Color = (Color(0.2f, 0.2f, 0.2f) + c.normalize).normalize

    def normalizedColor( t: Material.Type, isec: Intersection ): Color = normalizedColor( isec.shape.material.surface.color( t ) )

    /**
      * 光源以外の表面に衝突する度に再帰的に表面からフォトンを発射する。
      * 衝突の際に指定した関数が呼ばれ、戻り値がBadの場合、フォトンは発射されずに終了する。
      * また、光源に衝突した場合は、フォトンの追跡は終了する。
      * @param a 初期値
      * @param func 衝突の際に呼ばれる関数。
      * @param ray 入射光
      */
    def shootPhotonRecursively[A]( a: A )( croot: RenderableTree, func: (A, Material.Type, Intersection) => Result[A, A] , ray: Photon ): A = croot.nearestIntersection( ray ) match {
      // 何にも衝突しなかった場合
	  case None => a
      // 何かに衝突した場合
      case Some( i ) =>
        val t = i.shape.material.surface.random // 表面の選択
        func( a, t, i ) match {
          case Empty => a
          case Bad( a ) => a
          case Good( a ) => if( t == Material.Emittance ) a else shootPhotonRecursively( a )( croot, func, t match {
            case Material.Reflectance => i.createReflection
            case Material.Refractive => i.createRefraction( Renderer.medium( i, croot ) )
            case _ => i.createPhotonOnDiffuse
          })
      }
    }

  }

  trait PhotonMapper extends Renderer {

    val camera: Camera
    val root: RenderableTree

    /**
      * フォトンが反射・屈折する回数
      */
    val numberOfTimesToTrace: Int
    val numberOfPhotons: Int
    val radius: Float

    lazy val croot = root toCamera camera
    lazy val lights = croot.extractLights

    /**
      * nphotons個のフォトンでフォトンマップを作成する
      */
    def createPhotonMap: PhotonMap[PhotonMap.Octree] = {

      val map = PhotonMap.Octree( croot.getBoundingSphere )

      for( i <- 1 to numberOfPhotons ) {
        if( i%1000 == 0 ) println( ">> %d".format( i ) )
        lights.foreach { l =>
	      PathTracer.shootPhotonRecursively[(Color, Int)]( (Color.White, 0) )( croot, { case ((col2, cnt), _type, isec) =>
            // 衝突点の色
            val col = _type match {
              case Material.Emittance => isec.shape.material.surface.emissionColor
              case _ => PathTracer.normalizedColor( _type, isec )
            }

            // 前の光の色と合成する
            val col3 = (col2 * col).normalize

            // 拡散面の時だけマップに記録する
            val cnt2 = _type match {
              case Material.Diffusivity =>
                map( isec.point ) = col3
                cnt + 1
              case _ => cnt
            }

            Result( (col3, cnt2), cnt2 < numberOfTimesToTrace )
	      }, l.createPhoton( Color.White ) )
        }
      }

      map
    }

    def readPhotonMap( fname: String ): PhotonMap[PhotonMap.Octree] = PhotonMap.Octree( fname )

    def trace( map: PhotonMap[PhotonMap.Octree], radius: Float, weight: Float, ray: Photon ): Color = {
      RayTracer.trace( croot, ray, 0, numberOfTimesToTrace, { isec =>
        //map.sum( BoundingSphere( isec.point, radius ) ) / weight
        map.sum( isec.point, isec.normal, radius, radius/4 ) / weight
      } )
    }

    def render( map: PhotonMap[PhotonMap.Octree], x0: Int, y0: Int, x1: Int, y1: Int ): Unit = {
      val r1 = croot.getBoundingSphere.radius
      val weight = numberOfPhotons * (radius*radius)/(r1*r1) * 10

      for( y <- y0 until y1; x <- x0 until x1  ) {
        if( x == 0 ) println( "x:%d, y:%d, w:%f".format( x, y, weight ) )
        camera.screen.image.update( x, y, trace( map, radius, weight, camera.createPhoton( x, y ) ) )
      }

    }

    def render( map: PhotonMap[PhotonMap.Octree] ): Unit = render( map, 0, 0, camera.screen.width, camera.screen.height )

    def render: Unit = render( createPhotonMap )

  }

}
