package raytracing

import raytracing.linearalgebra._

case class Point(x: Int, y: Int, z: Int) {
  import Ray._

  def sum = x + y + z

  def dot(other: Point) = (this * other).sum

  def *(other: Point): Point = {
    Point(x * other.x, y * other.y, z * other.z)
  }

  def dot(other: Vector) = (this * other).sum

  def *(other: Vector): Point = {
    Point(x * other.as(0), y * other.as(1), z * other.as(2))
  }


  def -(p: Point): linearalgebra.Vector = linearalgebra.Vector(x - p.x, y - p.y, z - p.z)
}
