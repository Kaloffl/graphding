
object Vec2 {

  val Origin: Vec2 = Vec2( 0,  0)
  val Left  : Vec2 = Vec2( 1,  0)
  val Right : Vec2 = Vec2(-1,  0)
  val Down  : Vec2 = Vec2( 0,  1)
  val Up    : Vec2 = Vec2( 0, -1)

  def dot(v1: Vec2, v2: Vec2): Double = v1.x * v2.x + v1.y * v2.y
  def length(v: Vec2): Double = Math.sqrt(v.x * v.x + v.y * v.y)
  def normalize(v: Vec2): Vec2 = {
    if (v != Origin) {
      v / length(v)
    } else {
      Origin
    }
  }

  def turn_ccw(v: Vec2): Vec2 = Vec2(v.y, -v.x)
  def turn_cw (v: Vec2): Vec2 = Vec2(-v.y, v.x)
}

case class Vec2(x: Double, y: Double) {
  def *(f: Double): Vec2 = Vec2(x * f, y * f)
  def /(f: Double): Vec2 = Vec2(x / f, y / f)
  def +(v: Vec2): Vec2 = Vec2(x + v.x, y + v.y)
  def -(v: Vec2): Vec2 = Vec2(x - v.x, y - v.y)
}


object Circle {

  def apply(cx: Double, cy: Double, radius: Double): Circle = Circle(Vec2(cx, cy), radius)

  def contains_point(circle: Circle, point: Vec2): Boolean = {
    return Vec2.length(circle.center - point) <= circle.radius
  }
}

case class Circle(center: Vec2, radius: Double) {

}


object Rectangle {

  def apply(x1: Double, y1: Double, x2: Double, y2: Double): Rectangle = Rectangle(Vec2(x1, y1), Vec2(x2, y2))

  def contains_point(rectangle: Rectangle, point: Vec2): Boolean = {
    return rectangle.left <= point.x && point.x <= rectangle.right &&
           rectangle.top  <= point.y && point.y <= rectangle.bottom
  }
}

case class Rectangle(p1: Vec2, p2: Vec2) {
  def left  : Double = Math.min(p1.x, p2.x)
  def right : Double = Math.max(p1.x, p2.x)
  def top   : Double = Math.min(p1.y, p2.y)
  def bottom: Double = Math.max(p1.y, p2.y)

  def center: Vec2 = (p1 + p2) / 2

  def width : Double = right - left
  def height: Double = bottom - top
}