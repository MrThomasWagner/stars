package raytracing.linearalgebra

trait Linear {
  def *(other: Linear): Linear = (this, other) match {
    case (v: Vector, m: Matrix) =>
      Vector(m.cols.map(c => c dot v))
    case (m1: Matrix, m2: Matrix) =>
      Matrix(m1.rows.zip(m2.cols).map {
        case (a, b) => a * b
      })
    case _ => ???
  }
}
