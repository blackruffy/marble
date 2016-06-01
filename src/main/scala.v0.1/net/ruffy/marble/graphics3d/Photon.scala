package net.ruffy.marble.graphics3d {
  import net.ruffy.marble.graphics2d.{ Color }
  import net.ruffy.marble.math._
  import net.ruffy.util._
  import Material._
  import Implicits._

  /**
    * フォトンを表現するクラス。
    * @param medium このフォトンが存在する媒質
    * @param materials フォトンが通ってきた材質のスタック。屈折率を使用するため。
    */
  class Photon( val origin: Vector3, val direction: Vector3, val color: Color, val medium: Medium, val parent: Option[Photon] ) {

    //val curr = materials.headOption.map( _.refractiveIndex )
    //val prev = if( materials.isEmpty ) 1f else materials.tail.headOption.map( _.refractiveIndex ).getOrElse(1f)

    //def inside_?( n: Vector3 ) = (direction angle n) < Math.Pi_2

    //def createReflection( p: Vector3, n: Vector3 ): Photon =  if( inside_?(n) ) { 
    //  Photon( p, direction.reflect( -n ), materials )
    //} else {
    //  Photon( p, direction.reflect( n ), materials )
    //}

    /**
      * 屈折光を作成する。
      * @param m1 入射側衝突面の媒質
      * @param m2 射出側衝突面の媒質
      * @param p 衝突面
      * @param n 衝突面の法線。入射側に向いている必要がある。
      * @return 屈折光
      */
    //def createRefraction( m1: Medium, m2: Medium, p: Vector3, n: Vector3 ): Photon = { // outgoing
    //  val u = direction.refract( n, m1.refractiveIndex, m2.refractiveIndex )
    //  if( ( n angle u) > Math.Pi_2 ) Photon( p, u, if( materials.isEmpty ) Nil else materials.tail )
    //  else Photon( p, direction.reflect( n ), materials )
    //}

    //def createRefraction( m: Medium, p: Vector3, n: Vector3 ): Photon = if( inside_?(n) ) { // outgoing
    //  val u = direction.refract( -n, curr.getOrElse(m.refractiveIndex), prev )
    //  if( ( -n angle u) > Math.Pi_2 ) Photon( p, u, if( materials.isEmpty ) Nil else materials.tail )
    //  else Photon( p, direction.reflect( -n ), materials )
    //} else {
    //  val u = direction.refract( n, curr.getOrElse(1f), m.refractiveIndex )
    //  if( (n angle u) > Math.Pi_2 ) Photon( p, u, m :: materials )
    //  else Photon( p, direction.reflect( n ), materials )
    //}

//    /**
//      * フォトンが物体の表面に衝突した際に材質によってランダムにそのフォトンの次の行き先が決定される。
//      * @param m 衝突面の材質
//      * @param p 衝突点
//      * @param n 衝突点の法線
//      * @return フォトンを作成するためにランダムに選ばれた材質と衝突面から新たに発射されるフォトン。
//      */
//    def scatter( m: Material, p: Vector3, n: Vector3 ): (Material.Type, Option[Photon]) = {
//
//      def select( x: Material.Type ): (Material.Type, Option[Photon]) = (x, x match {
//        case Diffusivity => None
//        case Absorptance => None
//        case Reflectance => Some( createReflection( p, n ) )
//        case Refractive  => Some( createRefraction( m, p, n ) )
//        case Emittance   => None
//      } )
//
//      m.surface.random |> select
//    }
//
//    def scatter( i: Intersection ): (Material.Type, Option[Photon]) = scatter( i.material, i.point, i.normal )

    /**
      * 衝突時に選択された材質の種類によって反射光か屈折光を生成する。
      * @param p 衝突点
      * @param n 衝突点の法線
      * @param m 材質
      * @param x 材質の種類
      * @return 衝突後のフォトン
      */
    //def scatter( p: Vector3, n: Vector3, m: Medium, x: Material.Type ): Option[Photon] = x match {
    //  case Diffusivity => None
    //  case Reflectance => Some( createReflection( p, n ) )
    //  case Refractive  => Some( createRefraction( m, p, n ) )
    //  case Emittance   => None
    //}
    //
    //def scatter( i: Intersection, x: Material.Type ): Option[Photon] = scatter( i.point, i.normal, i.material, x )

  }

  object Photon {

    //def composeColor( c1: Color, c2: Color ): Color = c1 * c2
    def composeColor( c1: Color, c2: Color ): Color = c2

    def apply( origin: Vector3, direction: Vector3, color: Color, medium: Medium, parent: Option[Photon] ): Photon = new Photon( origin, direction, color, medium, parent )

    def apply( origin: Vector3, direction: Vector3, color: Color, medium: Medium ): Photon = new Photon( origin, direction.normalize, color, medium, None )


    def apply( p: Vector3, n: Vector3, theta1: Float, phi1: Float, color: Color, medium: Medium, parent: Option[Photon] ): Photon = {
      val v = n.normalize
      val (r, theta0, phi0) = v.toPolar

      val theta = theta0 + theta1
      val v1 = Vector3( Math.sin( theta ) * Math.cos( phi0 ), Math.cos( theta ), Math.sin( theta ) * Math.sin( phi0 ) )

      Photon( p, Matrix4x4.rotation( v, phi1 ) * v1, color, medium, parent )
    }

    def createPhoton( p: Vector3, n: Vector3, theta1: Float, phi1: Float, color: Color, medium: Medium, parent: Option[Photon] ): Photon = {
      val ny = n.normalize
      val nz = (ny x Vector3.Ny).normalize
      val nx = (nz x ny).normalize

      val v0 = (Matrix4x4.rotation( -ny, phi1 ) * nx).normalize
      val v1 = (v0 x ny).normalize
      val v2 = (Matrix4x4.rotation( v1, Math.Pi_2 - theta1 ) * v0).normalize

      //println((">>>", (v2 angle ny).toDegrees))

      Photon( p, v2, color, medium, parent )
    }

    /**
      * 法線ベクトルnに対する角度で指定した範囲内でランダムにフォトンを生成する。
      */
    def createRandomPhoton(
      th0: Float,
      th1: Float,
      ph0: Float,
      ph1: Float,
      p: Vector3,
      n: Vector3,
      color: Color,
      medium: Medium,
      parent: Option[Photon]
    ): Photon = {

      println("--------")
      println((th0.toDegrees, th1.toDegrees, ph0.toDegrees, ph1.toDegrees))

      val (s0, s1) = Math.min( Math.pow2( Math.cos(th0) ), Math.pow2( Math.cos(th1) ) )
      val (t0, t1) = Math.min( ph0/Math.Pix2, ph1/Math.Pix2 )

      val s = Math.random * (s1 - s0) + s0
      val t = Math.random * (t1 - t0) + t0

      val theta1 = Math.acos( Math.sqrt( s ) )
      val phi1 = t * Math.Pix2

      println((theta1.toDegrees, phi1.toDegrees))

      createPhoton( p, n, theta1, phi1, color, medium, parent )
    }

    /**
      * 点pから法線nが作る半球上のランダムに選んだ点に向うフォトンを作成する。
      * @param color 作成されるフォトンが保持する色。
      * @param medium 作成されるフォトンが向う媒質。
      */
    def createRandomPhoton( p: Vector3, n: Vector3, color: Color, medium: Medium, parent: Option[Photon] ): Photon = {
      val theta1 = Math.acos( Math.sqrt( Math.random ) )
      val phi1 = Math.random * Math.Pix2

      apply( p, n, theta1, phi1, color, medium, parent )
    }

  }

}
