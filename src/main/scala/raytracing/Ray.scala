package raytracing

import raytracing.linearalgebra._
import raytracing.structures.{Sphere, Structure}

class Ray(origin: Point, direction: EuclideanVector) {

  import Ray._

  def intersect(structure: Structure): Option[Double] = structure match {
    case s: Sphere =>
      val originToSphere = origin - s.origin
      solveQuadratic(
        1,
        2 * (originToSphere dot direction),
        (originToSphere dot originToSphere) - (s.radius * s.radius)
      ) match {
        case (Some(a), Some(b)) => Some(a min b)
        case (Some(a), None) => Some(a)
        case _ => None
      }
  }
}

object Ray {
  def solveQuadratic(a: Double, b: Double, c: Double): (Option[Double], Option[Double]) = {
    def delta = (b * b) - (4 * a * c)

    if (delta < 0) (None, None)
    else if (delta == 0) (Some(-b / (2 * a)), None)
    else {
      (Some((-b + math.sqrt(delta)) / (2 * a)),
        Some((-b - math.sqrt(delta)) / (2 * a)))
    }
  }
}