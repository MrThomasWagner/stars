package raytracing
import raytracing.image._
import raytracing.linearalgebra.{EuclideanVector, Point}
import raytracing.structures.Structure
import raytracing.structures.Sphere

class World(ss: List[Structure]) {
  implicit val ordering: Ordering[(Structure, Double)] = new Ordering[(Structure, Double)]{
    override def compare(x: (Structure, Double), y: (Structure, Double)): Int = {
      if (x._2 < y._2) 1
      else 0
    }
  }
  def trace(ray: Ray): Pixel = {
    val m: List[(Structure, Double)] = ss.flatMap { struct =>
      ray.intersect(struct) match {
        case Some(distance) => List((struct, distance))
        case None => Nil
      }
    }
    if (m.isEmpty) Pixel(0, 0, 0)
    else m.min._1.color()
  }

}

object World extends App {
  val eye = Point(0, 0, 0)

  val (width, height) = (1600, 400)

  val film = new Film((width, height), -1000)

  val world = new World(List(new Sphere(Point(0, -50, -800), 250)))

  for(
    x <- 0 until width;
    y <- 0 until height
  ) {
    val direction: EuclideanVector = film.coordinatesFor(x, y) - eye
    film.set(x, y, world.trace(new Ray(eye, direction.normal())))
  }

  film.write("/Users/tommy/Desktop/wtf.jpg")
}
