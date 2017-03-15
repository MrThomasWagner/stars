package raytracing.structures

import raytracing.image.Pixel
import raytracing.linearalgebra.Point

case class Sphere(origin: Point, radius: Int) extends Structure {
  override def color(): Pixel = Pixel(150, 0, 255)
}
