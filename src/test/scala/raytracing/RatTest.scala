package raytracing

import org.scalatest.FlatSpec
import org.scalatest._

class RayTest extends FlatSpec {
  "solveQuadratic" should "calculate the roots of quadratic equations " in {
    val roots = Ray.solveQuadratic(5, 6, 1)
    assert(roots == (Some(-.2), Some(-1)))
  }
}
