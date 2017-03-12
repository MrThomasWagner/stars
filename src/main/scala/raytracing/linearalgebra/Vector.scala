package raytracing.linearalgebra


case class Vector(as: Array[Double]) extends Linear {
  val length: Int = as.length

  lazy val sum: Double = as.sum

  def apply(i: Int): Double = as(i)

  def dot(other:Vector) = (this * other).sum
  def *(other: Vector): Vector = {
    require(length == other.length, "Incompatible vector multiplication, vectors must be of the same size")
    Vector(as.zip(other.as).map(a => a._1 * a._2))
  }

  override def toString: String = as.mkString("< ", ", ", " >")
}

object Vector {
  def apply(ds: Double*): Vector = Vector(ds.toArray)

}
