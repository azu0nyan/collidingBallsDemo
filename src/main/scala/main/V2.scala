package main

object V2 {
  def apply(s:Double): V2 = from(s, s)

  def from(x:Double, y:Double) :V2 = V2(x, y)

  implicit def pairToV2(p: (Double, Double)): V2 = V2(p._1, p._2)

  implicit def fToV2(f: Double): V2 = V2(f, f)


  implicit def toUnit(v: V2): UnitV2 = new UnitV2(v)

  val ox: V2 = V2(1, 0)

  val oy: V2 = V2(0, 1)

  val ZERO : V2= V2(0, 0)




}

case class V2(x: Double, y: Double) {


  def apply(i: Int): Double = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
  }

  def xInt:Int = x.toInt

  def yInt:Int = y.toInt

  def unary_- : V2 = opposite

  def opposite: V2 = V2(-x, -y)

  def +(v: V2): V2 = V2(x + v.x, y + v.y)

  def -(v: V2): V2 = V2(x - v.x, y - v.y)

  def *(v: V2): V2 = V2(x * v.x, y * v.y)

  def *(s:Double): V2 = V2(x * s, y * s)

  def /(v: V2): V2 = V2(x / v.x, y / v.y)

  def -(): V2 = V2(-x, -y)

  def **(v: V2): Double = x * v.x + y * v.y

  def det(v: V2): Double = x * v.y - y * v.x

  def normalize: V2 = if (length == 0) {
    V2(0, 0)
  } else {
    /(length)
  }

  //this -> ot
  def proj(ot:V2):Double = (this ** ot) / ot.length

  def angle(v: V2): Double = (math.atan2(v.y, v.x) - math.atan2(y, x))

  def distance(v: V2): Double = (this - v).length

  def rotate90CW:V2 = V2(-y, x)

  def rotate90CCW:V2 = V2(y, -x)

  def rotate(a: Double):V2 = V2(x * math.cos(a) - y * math.sin(a), x * math.sin(a) + y * math.cos(a))

  def rotateAroundPoint(rotation: Double, point: V2): V2 = (this - point).rotate(rotation) + point

  def scaleAroundPoint(scale: Double, point: V2): V2 = (this - point) * scale + point

  def lengthSquared:Double = this ** this

  def length: Double = math.hypot(x, y)

  override def toString: String = s"""V2($x, $y)"""

  def toSeq: Seq[Double] = Seq(x, y)


}

class UnitV2(v: V2) extends V2(v.normalize.x, v.normalize.y)
