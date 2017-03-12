package raytracing.linearalgebra

case class Matrix(private val entries: Array[Array[Double]]) extends Linear {
  val height: Int = entries.length
  val width: Int = entries.head.length
  require(entries.forall(_.length == width))
  lazy val rows: Array[Vector] = entries.map(row => Vector(row))
  lazy val cols: Array[Vector] = (0 until width).map(col).toArray

  def col(i: Int): Vector = Vector(entries.map(c => c(i)))

  def row(i: Int): Vector = Vector(entries(i))

  def apply(x: Int, y: Int): Double = entries(x)(y)

  override def toString: String = entries.map(r => r.mkString("| ", ", ", " |")).mkString("\n")
}

object Matrix {
  implicit val asDouble: (Int => Double) = _.toDouble
  implicit val asDoubleIterable: (List[Int] => List[Double]) = _.map(_.toDouble)

  def apply(vectors: Array[Vector]): Matrix = Matrix(vectors.map(_.as))

  def apply(iterables: Iterable[Double]*): Matrix = Matrix(iterables.map(_.toArray).toArray)
}

object MatrixMain extends App {
  import Matrix._

  println(Matrix(List(1, 2), List(3, 4)))

  println(Vector(1, 2, 3) * Matrix(
    List(1, 1, 1),
    List(1, 1, 1),
    List(1, 1, 1)
  ))
}

