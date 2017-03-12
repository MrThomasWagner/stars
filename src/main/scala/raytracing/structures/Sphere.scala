package raytracing.structures

import raytracing.Point
import raytracing.image.Pixel

case class Sphere(origin: Point, radius: Int) extends Structure {
  override def color(): Pixel = Pixel(150, 0, 255)
}
