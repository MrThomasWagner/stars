package matrix

sealed trait Linear {
  def *(other: Linear): Linear = (this, other) match {
    case (v: Vector, m: Matrix) =>
      Vector(m.cols.map(c => c * v).map(_.sum))
    case (m1: Matrix, m2: Matrix) =>
      Matrix(m1.rows.zip(m2.cols).map {
        case (a, b) => a * b
      })
    case _ => ???
  }
}

case class Vector(as: Array[Double]) extends Linear {
  val length: Int = as.length

  lazy val sum: Double = as.sum

  def apply(i: Int): Double = as(i)

  def *(other: Vector): Vector = {
    require(length == other.length, "Incompatible vector multiplication, vectors must be of the same size")
    Vector(as.zip(other.as).map(a => a._1 * a._2))
  }

  override def toString: String = as.mkString("< ", ", ", " >")
}

object Vector {
  def apply(ds: Double*): Vector = Vector(ds.toArray)

}

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

