package object examples {

  import net.ruffy.marble.graphics2d.{ Color }
  import net.ruffy.marble.graphics3d.{ Material, Branch, Leaf, RenderableTree }
  import net.ruffy.marble.graphics3d.shapes.{ Rectangle, BoundingSphere, Light }
  import net.ruffy.marble.math.{ Vector3, Matrix4x4 }
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

    def apply( width: Float, height: Float, depth: Float ): RenderableTree = apply(
      Vector3( -width/2f,  height/2f,  depth/2f ), // 0 up, left, front
      Vector3(  width/2f,  height/2f,  depth/2f ), // 1 up, right, front
      Vector3(  width/2f, -height/2f,  depth/2f ), // 2 bottom, right, front
      Vector3( -width/2f, -height/2f,  depth/2f ), // 3 bottom, left, front
      
      Vector3( -width/2f,  height/2f, -depth/2f ), // 4 up, left, rear
      Vector3(  width/2f,  height/2f, -depth/2f ), // 5 up, right, rear
      Vector3(  width/2f, -height/2f, -depth/2f ), // 6 bottom right, rear
      Vector3( -width/2f, -height/2f, -depth/2f )  // 7 bottom, left, rear
    )

  }

  lazy val cornelBox = Branch(
    Leaf( Light.Rectangle(
      Vector3(  1.0f, 1.99f,  1.0f ),
      Vector3( -1.0f, 1.99f,  1.0f ),
      Vector3( -1.0f, 1.99f, -1.0f ),
      Vector3(  1.0f, 1.99f, -1.0f )
    ) ) ,
    CornelBox( 5f, 4f, 8f ),
    Leaf( BoundingSphere(
      0.8f,
      Reflection( Color.White )
    ).transform( Matrix4x4.translate( 1.2f, -1.2f, -1.0f ) ) ),
    Leaf( BoundingSphere(
      0.8f,
      Material( Surface( None, Some(0.1f, Color.White), Some(0.9f, Color.White), None ), Medium( 1.5f ) )
    ).transform( Matrix4x4.translate( -1.2f, -1.2f, 1.0f ) ) )
  )

}
