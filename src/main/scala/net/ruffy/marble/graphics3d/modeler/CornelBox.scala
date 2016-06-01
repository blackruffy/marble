package net.ruffy.marble.graphics3d.modeler {

  import net.ruffy.marble.graphics2d.{ ColorEnv }
  import net.ruffy.marble.graphics3d.renderer.{ MaterialEnv, RenderableTreeEnv }
  import net.ruffy.marble.graphics3d.modeler.{ RectangleEnv, BoundingSphereEnv, LightEnv, Shape3DEnv }
  import net.ruffy.marble.math.{ VectorEnv, MatrixEnv, MathEnv, NumberEnv }

  trait CornelBoxEnv {

    env: ColorEnv
        with MaterialEnv
        with RenderableTreeEnv
        with RectangleEnv
        with BoundingSphereEnv
        with Shape3DEnv
        with LightEnv
        with VectorEnv
        with MatrixEnv
        with MathEnv
        with NumberEnv =>

    import Material.{ Medium, Surface, Reflection, Diffusion }

    object CornelBox {

      def apply(
        p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3,
        p4: Vector3, p5: Vector3, p6: Vector3, p7: Vector3
      ): RenderableTree = Branch(
        Leaf( Rectangle( p1, p0, p4, p5, Diffusion(Color.White) ) ), // top
        Leaf( Rectangle( p3, p2, p6, p7, Diffusion(Color.White) ) ), // bottom
        Leaf( Rectangle( p4, p0, p3, p7, Diffusion(Color.Blue)  ) ), // left
        Leaf( Rectangle( p2, p1, p5, p6, Diffusion(Color.Green) ) ), // right
        Leaf( Rectangle( p5, p4, p7, p6, Diffusion(Color.Red)   ) )  // rear
      )

      def apply( width: Double, height: Double, depth: Double ): RenderableTree = apply(
        Vector3( -width/2.0,  height/2.0,  depth/2.0 ), // 0 up, left, front
        Vector3(  width/2.0,  height/2.0,  depth/2.0 ), // 1 up, right, front
        Vector3(  width/2.0, -height/2.0,  depth/2.0 ), // 2 bottom, right, front
        Vector3( -width/2.0, -height/2.0,  depth/2.0 ), // 3 bottom, left, front
        
        Vector3( -width/2.0,  height/2.0, -depth/2.0 ), // 4 up, left, rear
        Vector3(  width/2.0,  height/2.0, -depth/2.0 ), // 5 up, right, rear
        Vector3(  width/2.0, -height/2.0, -depth/2.0 ), // 6 bottom right, rear
        Vector3( -width/2.0, -height/2.0, -depth/2.0 )  // 7 bottom, left, rear
      )

    }

    lazy val cornelBox = Branch(
      Leaf( Light.Rectangle(
        Vector3(  1.0, 1.99,  1.0 ),
        Vector3( -1.0, 1.99,  1.0 ),
        Vector3( -1.0, 1.99, -1.0 ),
        Vector3(  1.0, 1.99, -1.0 )
      ) ) ,
      CornelBox( 5, 4, 5 ),
      Leaf( BoundingSphere(
        0.8,
        Reflection( Color.White )
      ).transform( Matrix4x4.translate( 1.2, -1.2, -1.0 ) ) ),
      Leaf( BoundingSphere(
        0.8,
        Material( Surface( None, Some(0.1, Color.White), Some(0.9, Color.White), None ), Medium( 1.5 ) )
      ).transform( Matrix4x4.translate( -1.2, -1.2, 1.0 ) ) )
    )

  }

}
