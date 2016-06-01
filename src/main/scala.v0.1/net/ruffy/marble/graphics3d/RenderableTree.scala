package net.ruffy.marble.graphics3d {

  import net.ruffy.util.{
    Tree,
    BasicBranch,
    BasicLeaf,
    Implicits
  }

  import Implicits._
  import net.ruffy.marble.math.{ Matrix4x4, Math, Vector3 }
  import net.ruffy.marble.graphics2d.{ Color, Graphics, ZBuffer }
  import Material._
  import shapes._

  abstract class RenderableTree extends Tree[Shape3D] {

    def center = Shape3D.getCenter[RenderableTree]( this, (xs, a, f) => xs.foldLeft( a )( f ) )

    def getBoundingSphere = Shape3D.getBoundingSphere[RenderableTree]( this, center, (xs, a, f) => xs.foldLeft( a )( f ) )

    def drawWireFrame( g: Graphics, camera: Camera, color: Int ): Unit = foreach( _.drawWireFrame( g, camera, color ) )

    def fill( g: Graphics, camera: Camera, light: Light ): Unit = fill( g, camera, light :: Nil )

    def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = toList.sortWith( (a, b) => a.center.z > b.center.z ).foreach( _.fill( g, camera, lights ) )

    def fillWithGouraudShading( g: Graphics, camera: Camera, light: Light ): Unit = fillWithGouraudShading( g, camera, light :: Nil )

    def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light] ): Unit = foreach( _.fillWithGouraudShading( g, camera, lights, camera.screen.image.zbuffer ) )

    def map( f: Shape3D => Shape3D ): RenderableTree = map( f, new RenderableBranch, (s:Shape3D) => new RenderableLeaf( s ) ).asInstanceOf[RenderableTree]

    def transform( m: Matrix4x4 ): RenderableTree = map( _ transform m )

    def toCamera( camera: Camera ): RenderableTree = map( _ toCamera camera )

    def extractLights: List[Light] = foldLeft[List[Light]]( Nil ) {
	  case (lights, s: Shape3D) => s match {
        case x: Light => x :: lights
        case _ => lights
	  }
	  case (lights, _) => lights
    }

    def nearestIntersection( ray: Photon ): Option[Intersection] = foldLeft[Option[Intersection]]( None ) {
	  case (None, s: Shape3D ) => s.intersect( ray )
	  case (i0, s: Shape3D) => s.intersect( ray ) |> Intersection.near(i0)
	  case (_, _) => None
    }

    /**
      * 点pが含まれる最も内側のオブジェクトの媒質を取得する。
      */
    def medium( p: Vector3, exclude: List[Shape3D] = Nil ): Medium = foldLeft[Option[Shape3D]]( None ) {
      case (None, b) => if( !exclude.exists( _ eq b ) && (b inside_? p) ) Some( b ) else None
      case (Some(a), b) =>
        val ea = !exclude.exists( _ eq a )
        val eb = !exclude.exists( _ eq b )
        if( ea && eb ) {
          val ia = a inside_? p
          val ib = b inside_? p
          if( ia && ib ) Some( if( a < b ) a else b )
          else if( ia ) Some( a )
          else if( ib ) Some( b )
          else None
        }
        else if( ea && (a inside_? p) ) Some( a )
        else if( eb && (b inside_? p) ) Some( b )
        else None
    } match {
      case Some( s ) => s.material.medium
      case _ => Medium.Default
    }

    //def shadow_?( light: Light, isect: Intersection ): Boolean = nearestIntersection( Photon( isect.point, (isect.point -> light.center).normalize ) ) match {
	//  case Some( x ) => x.material.isLight
	//  case _ => false
    //}

    //def traceRay( ray: Photon, light: Light, cnt: Int = 0 ): Color = if( cnt < 5 ) {
    //  nearestIntersection( ray ).map { i =>
	//    val (reflIdx, refrIdx, diffIdx, emitIdx) = i.material |> { x => (x.reflectance, x.refractiveRate, x.diffusivity, x.emittance) }
	//    val cl = if( reflIdx > 0 ) traceRay( ray.createReflection( i.point, i.normal ), light, cnt +1 ) else Color.Black
	//    val cr = if( refrIdx > 0 ) traceRay( ray.createRefraction( i.material, i.point, i.normal ), light, cnt +1 ) else Color.Black
	//    val cd = if( diffIdx > 0 ) {
	//      if( shadow_?( light, i ) ) Color.Black
	//      else light.color( i.normal, i.point, i.material, Material.Diffusivity )
	//    } else Color.Black
	//    val ce = if( emitIdx > 0 ) i.material.emissionColor else Color.Black
	//    cl * reflIdx + cr * refrIdx + cd * diffIdx + ce * emitIdx
    //  }.getOrElse( Color.Black )
    //} else Color.Black

    //def shootPhoton( ray: Photon, cnt: Int = 0 ): Option[(Material.Type, Intersection)] = if( cnt < 5 ) {
    //  nearestIntersection( ray ).flatMap { i =>
    //    val t = i.material.random
    //    ray.scatter(i, t) match {
	//      case Some(r) => shootPhoton( r, cnt + 1 )
	//      case Some(r)  => shootPhoton( r, cnt + 1 )
	//      case _ => Some((t, i))
    //    }
    //  }
    //} else None
    
    /**
      * 拡散面に衝突する度に再帰的にランダムにフォトンを発射する。
      * 衝突の際に指定した関数が呼ばれ、戻り値にfalseが含まれていれば、フォトンは発射されずに終了する。
      */
    //def shootPhotonRecursively[A]( a: A )( func: (A, Material.Type, Intersection) => (A, Boolean) , ray: Photon ): A = shootPhoton( ray ) match {
	//  case None => a
	//  case Some( ( Emittance, i ) ) => func( a, Emittance, i )._1
	//  case Some( ( t, i ) ) =>
    //    val (res, b) = func( a, t, i )
    //    if( b ) shootPhotonRecursively( res )( func, i.createPhotonOnDiffuse ) else res
    //}

    //def shootPhotonRecursively( ray: Photon, num: Int ): List[Intersection] = {
    //  shootPhotonRecursively[(List[Intersection], Int)]((Nil, 0))( { case ((is, cnt), i) => ((i :: is, cnt+1), cnt < num) }, ray )._1
    //}

    //def shootPhotonRecursively( ray: Photon, num: Int, color: (Material.Type, Intersection) => Color ): Color = {
	//  shootPhotonRecursively[(Color, Material.Type, Int)]((Color.Black, Absorptance, 0))( {
    //    case ((c, _, cnt), t:Material.Emittance.type, i) => ((i.material.color, t, cnt + 1), cnt < num)
    //    case ((c, _, cnt), t, i) => ((if( cnt == 0 ) color( t, i ) else c + color( t, i ) * Math.pow(0.5f, cnt), t, cnt + 1), cnt < num)
	//  }, ray ) |> { case (c, _, _ ) => c }
    //}

  }
  
  class RenderableBranch extends RenderableTree with BasicBranch[Shape3D]
  
  case class RenderableLeaf( element: Shape3D ) extends RenderableTree with BasicLeaf[Shape3D] {
    lazy val shape = element
  }

  object Branch {

    def apply( children: RenderableTree* ): RenderableTree = {
      val b = new RenderableBranch
      b( children.toList )
      b
    }

  }

  object Leaf {

    def apply( shape: Shape3D ): RenderableTree = RenderableLeaf( shape )

  }

}
