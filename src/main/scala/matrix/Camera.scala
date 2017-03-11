package matrix

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable.ArrayBuffer

case class Pixel(r: Int, g: Int, b: Int)

class Camera(aspectRatio: Tuple2[Int, Int], depth: Int) {
  val origin: Point = Point(0, 0, 0)
  private val (width, height) = aspectRatio


  val rows: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
  (0 to height).foreach { i => rows += Pixel(i* (255/height), 0, 0) }
  val film: ArrayBuffer[ArrayBuffer[Pixel]] = ArrayBuffer.fill(width, height) {
   Pixel(0, 0, 0)
  }

  def coordsFor(x: Int, y: Int): Point = {
    Point(x - width / 2, y - height / 2, -depth)
  }


  def write() = {
    val out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    (0 until width).foreach { x =>
      (0 until height).foreach { y =>
        val pixel = film(x)(y)

        val red: Double = x*255.toDouble/width
        val color:Int = red.toInt* 65536 + (pixel.g * 256) + pixel.b
        out.setRGB(x, y, color)
      }
    }
    val f = new File("/Users/tommy/Desktop/yay.jpeg")
    ImageIO.write(out, "jpg", f)
  }

}

object wtf extends App {
  new Camera((1000, 1000), 5).write()
}
