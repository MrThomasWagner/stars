package raytracing.linearalgebra

trait Linear {
  def *(other: Linear): Linear = (this, other) match {
    case (v: EuclideanVector, m: Matrix) => EuclideanVector(m.cols.map(c => c dot v))
    case _ => ???
  }
}
