package matrix

case class Point(x: Int, y: Int, z: Int) {
  def -(p: Point): Vector = Vector(x - p.x, y - p.y, z - p.z)
}
