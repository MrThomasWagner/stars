package raytracing

import raytracing.linearalgebra._
import raytracing.structures.{Sphere, Structure}

class Ray(origin: Point, direction: Vector) {
  import Ray._
  def intersect(structure: Structure): Option[Double] = structure match {
    case s: Sphere =>
      solveQuadratic(
        direction dot direction,
        2 * (origin dot direction),
        ((origin-s.origin) dot (origin - s.origin)) - s.radius*s.radius
      ) match {
        case (Some(a), Some(b)) => Some(a min b)
        case (Some(a), None) => Some(a)
        case _ => None
      }
  }
}
object Ray {
  implicit val toDouble: (Double => Int) = _.toInt

  def solveQuadratic(a: Double, b: Double, c: Double): (Option[Double], Option[Double]) = {
    def delta = (b * b) - (4 * a * c)
    if (delta < 0) (None, None)
    else if (delta == 0) (Some(-b / (2*a)), None)
    else {
      (Some((-b + math.sqrt(delta))/ (2*a)),
        Some((-b - math.sqrt(delta))/ (2*a)))
    }
  }
}