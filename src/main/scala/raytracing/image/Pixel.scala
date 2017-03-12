package raytracing.image

/**
  * Created by tommy on 3/11/17.
  */
case class Pixel(r: Int, g: Int, b: Int) {
  def color: Int = {
    r * 65536 + g * 256 + b
  }
}
