package raytracing.linearalgebra

case class Matrix(private val entries: Vector[Vector[Double]]) extends Linear {
  val height: Int = entries.length
  val width: Int = entries.head.length
  require(entries.forall(_.length == width))
  lazy val rows: Vector[EuclideanVector] = entries.map(row => EuclideanVector(row))
  lazy val cols: Vector[EuclideanVector] = (0 until width).map(col).toVector

  def col(i: Int): EuclideanVector = EuclideanVector(entries.map(c => c(i)))

  def row(i: Int): EuclideanVector = EuclideanVector(entries(i))

  def apply(x: Int, y: Int): Double = entries(x)(y)

  override def toString: String = entries.map(r => r.mkString("| ", ", ", " |")).mkString("\n")
}

object Matrix {
  implicit val asDouble: (Int => Double) = _.toDouble
  implicit val asDoubleIterable: (List[Int] => List[Double]) = _.map(_.toDouble)
}
