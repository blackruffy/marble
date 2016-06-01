package net.ruffy.marble {

  import net.ruffy.marble.graphics2d.{
    ImageEnv,
    ColorEnv,
    GraphicsEnv,
    BufferedImageEnv
  }

  import net.ruffy.marble.graphics3d.renderer.{
    ScreenEnv,
    CameraEnv,
    DefaultCameraEnv,
    LambertCameraEnv,
    MaterialEnv,
    RenderableTreeEnv,
    RendererEnv,
    PhotonEnv,
    PhotonMapEnv,
    IntersectionEnv,
    OctreeEnv
  }

  import net.ruffy.marble.graphics3d.modeler.{
    RectangleEnv,
    BoundingSphereEnv,
    LightEnv,
    Shape3DEnv,
    SolidEnv,
    SphereEnv,
    PolygonEnv,
    PolygonsEnv,
    TriangleEnv,
    CornelBoxEnv,
    LineEnv,
    AxisEnv,
    BoxEnv
  }

  import net.ruffy.marble.math.{ DoubleEnv, MathEnv, VectorEnv, MatrixEnv, NumberEnv, CoordinateSystemEnv }

  trait Env
      extends MathEnv
      with CoordinateSystemEnv
      with NumberEnv
      with VectorEnv
      with MatrixEnv
      with ImageEnv
      with ColorEnv
      with GraphicsEnv
      with ScreenEnv
      with CameraEnv
      with DefaultCameraEnv
      with LambertCameraEnv
      with MaterialEnv
      with PhotonEnv
      with PhotonMapEnv
      with OctreeEnv
      with IntersectionEnv
      with RenderableTreeEnv
      with RendererEnv
      with BoundingSphereEnv
      with SolidEnv
      with RectangleEnv
      with SphereEnv
      with LightEnv
      with LineEnv
      with AxisEnv
      with BoxEnv
      with PolygonEnv
      with PolygonsEnv
      with TriangleEnv
      with Shape3DEnv
      with CornelBoxEnv
      with BufferedImageEnv

  object Env {

    object Double extends Env with DoubleEnv

  }
}
