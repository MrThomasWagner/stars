package raytracing.linearalgebra

case class Point(x: Double, y: Double, z: Double) extends EuclideanVector(Vector(x, y, z))

class EuclideanVector(val as: Vector[Double]) extends Linear {
  def apply(i: Int): Double = as(i)
  lazy val sum: Double = as.sum
  lazy val magnitude: Double = math.sqrt(this dot this)

  def -(other: EuclideanVector): EuclideanVector = EuclideanVector(as.zip(other.as).map(a => a._1 - a._2))
  def *(other: EuclideanVector): EuclideanVector = EuclideanVector(as.zip(other.as).map(a => a._1 * a._2))
  def *(scalar: Double): EuclideanVector = EuclideanVector(as.map(_*2))
  def dot(other: EuclideanVector): Double = (this * other).sum
  def normal(): EuclideanVector = if (magnitude == 1) this else EuclideanVector(as.map(_ / magnitude))

  override def toString: String = as.mkString("< ", ", ", " >")
}

object EuclideanVector {
  def apply(ds: Double*): EuclideanVector = EuclideanVector(ds.toVector)

  def apply(array: Vector[Double]): EuclideanVector = new EuclideanVector(array)

  def unapply(vec: EuclideanVector): Option[Vector[Double]] = Some(vec.as)
}
