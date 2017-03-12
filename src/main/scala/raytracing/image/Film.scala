package raytracing.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import raytracing.Point

class Film(aspectRatio: (Int, Int), depth: Int) {
  private val (width, height) = aspectRatio

  val out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  def set(x: Int, y: Int, p: Pixel): Unit = out.setRGB(x, y, p.color)

  def coordinatesFor(x: Int, y: Int) = Point(x - width / 2, y - height / 2, depth)

  def write(fileName: String) = ImageIO.write(out, "jpg", new File(fileName))
}

object wtf extends App {
  //  new Film((1000, 1000)).write("/Users/tommy/Desktop/output.jpg")
}
