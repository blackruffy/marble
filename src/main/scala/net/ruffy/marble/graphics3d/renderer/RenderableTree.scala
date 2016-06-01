package net.ruffy.marble.graphics3d.renderer {

  import net.ruffy.util.{
    Tree,
    BasicBranch,
    BasicLeaf,
    Implicits,
    Result,
    Good,
    Bad,
    Empty
  }

  import Implicits._
  import net.ruffy.marble.math.{ MatrixEnv, MathEnv, VectorEnv, NumberEnv }
  import net.ruffy.marble.graphics2d.{ ColorEnv, GraphicsEnv, ZBuffer }
  import net.ruffy.marble.graphics3d.modeler.{ Shape3DEnv, BoundingSphereEnv, LightEnv }

  trait RenderableTreeEnv {

    env: MatrixEnv
        with IntersectionEnv
        with PhotonEnv
        with LightEnv
        with CameraEnv
        with MaterialEnv
        with MathEnv
        with NumberEnv
        with VectorEnv
        with ColorEnv
        with GraphicsEnv
        with Shape3DEnv
        with BoundingSphereEnv =>

    import Material._

    abstract class RenderableTree extends Tree[Shape3D] {

      def center = Shape3D.getCenter[RenderableTree]( this, (xs, a, f) => xs.foldLeft( a )( f ) )

      def getBoundingSphere = Shape3D.getBoundingSphere[RenderableTree]( this, center, (xs, a, f) => xs.foldLeft( a )( f ) )

      def drawWireFrame( g: Graphics, camera: Camera, color: Int ): Unit = foreach {
        _.drawWireFrame( g, camera, color )
      }

      def fill( g: Graphics, camera: Camera, light: Light ): Unit = fill( g, camera, light :: Nil )

      def fill( g: Graphics, camera: Camera, lights: List[Light] ): Unit = toList.sortWith {
        (a, b) => a.center.z > b.center.z
      }.foreach( _.fill( g, camera, lights ) )

      def fillWithGouraudShading( g: Graphics, camera: Camera, light: Light ): Unit = {
        fillWithGouraudShading( g, camera, light :: Nil )
      }

      def fillWithGouraudShading( g: Graphics, camera: Camera, lights: List[Light] ): Unit = foreach{
        _.fillWithGouraudShading( g, camera, lights, camera.screen.image.zbuffer )
      }

      def map( f: Shape3D => Shape3D ): RenderableTree = map( f, new RenderableBranch, (s:Shape3D) =>
        new RenderableLeaf( s )
      ).asInstanceOf[RenderableTree]

      def transform( m: Matrix4x4 ): RenderableTree = map( _ transform m )

      def toCamera( camera: Camera ): RenderableTree = map( _ toCamera camera )

      def extractLights: List[Light] = foldLeft[List[Light]]( Nil ) {
	    case (lights, s: Shape3D) => s match {
          case x: Light => x :: lights
          case _ => lights
	    }
	    case (lights, _) => lights
      }

      def nearestIntersection( ray: Photon ): Result[Intersection, Intersection] = {
        foldLeft[Result[Intersection, Intersection]]( Empty ) {
	      case (Empty, s: Shape3D ) => s.intersect( ray )
	      case (i0, s: Shape3D) => s.intersect( ray ) |> Intersection.near(i0)
	      case (_, _) => Empty
        }
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
  
}
