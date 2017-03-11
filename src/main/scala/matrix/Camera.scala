package matrix

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable.ArrayBuffer

case class Pixel(r: Int, g: Int, b: Int) {
  def color: Int = {
    r * 65536 + g * 256 + b
  }
}

class Camera(aspectRatio: Tuple2[Int, Int], depth: Int) {
  val origin: Point = Point(0, 0, 0)
  private val (width, height) = aspectRatio

  val film = ArrayBuffer.fill(width, height)(Pixel(0, 0, 0))
  for (
    x <- 0 until width;
    y <- 0 until height
  ) {
    val green = x*(255.toDouble/width)
    film(x)(y) = Pixel(0, green.toInt, 0)
  }


  def coordsFor(x: Int, y: Int): Point = {
    Point(x - width / 2, y - height / 2, -depth)
  }

  def write() = {
    val out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (
      x <- 0 until width;
      y <- 0 until height
    ) yield {
      out.setRGB(x, y, film(x)(y).color)
    }
    ImageIO.write(out, "jpg", new File("/Users/tommy/Desktop/yay.jpeg"))
  }

}

object wtf extends App {
  new Camera((1000, 1000), 5).write()
}
