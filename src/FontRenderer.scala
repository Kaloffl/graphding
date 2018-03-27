package ui

import java.awt.image.BufferedImage
import java.io.File
import java.util.Scanner
import javax.imageio.ImageIO

import java.lang.Math._

object FontRenderer {

  case class CharData(
    char: Char,
    sx: Int, sy: Int,
    sw: Int, sh: Int,
    tx: Int, ty: Int,
    tw: Int, th: Int,
    page: Int)

  case class CharsetSdf(
    buffer_width: Int,
    buffer_height: Int,
    values: Array[Float]) {

    def distance_at(x: Int, y: Int): Float = {
      return values(x + y * buffer_width)
    }

    def distance_at(x: Float, y: Float): Float = {
      val ix = x.toInt
      val iy = y.toInt
      val fx = x - ix
      val fy = y - iy
      return (distance_at(ix, iy    ) * (1 - fx) + distance_at(ix + 1, iy    ) * fx) * (1 - fy) + 
             (distance_at(ix, iy + 1) * (1 - fx) + distance_at(ix + 1, iy + 1) * fx) * fy
    }
  }

  def load(font_file: String): FontData = {
    val scanner = new Scanner(new File(font_file))
    var pages = Seq[String]()
    var chars = Map[Char, CharData]()
    var kernings = Map[(Char, Char), Int]()
    var line_height = 0
    var base = 0
    var size = 0
    var paddings = new Array[Int](4)
    while (scanner.hasNextLine()) {
      val line = scanner.nextLine
      val parts = line.split("[ =]+")
      parts(0) match {
        case "info" =>
          val part_parts = parts(20).split(",")
          size = Integer.parseInt(parts(4))
          for (i <- 0 until 4) paddings(i) = Integer.parseInt(part_parts(i))
        case "common" =>
          line_height = Integer.parseInt(parts(2))
          base        = Integer.parseInt(parts(4))
        case "page" =>
          val filename = parts(4).substring(1, parts(4).length - 1)
          val path = font_file.substring(0, font_file.lastIndexOf("/") + 1) + filename
          pages = pages :+ path
        case "chars" =>
        case "char" =>
          val character = CharData(
            char = Integer.parseInt(parts(2)).toChar,
            sx   = Integer.parseInt(parts(4)),
            sy   = Integer.parseInt(parts(6)),
            sw   = Integer.parseInt(parts(8)),
            sh   = Integer.parseInt(parts(10)),
            tx   = Integer.parseInt(parts(12)),
            ty   = Integer.parseInt(parts(14)),
            tw   = Integer.parseInt(parts(16)),
            th   = Integer.parseInt(parts(10)),
            page = Integer.parseInt(parts(18)))
          chars += (character.char -> character)
        case "kernings" =>
        case "kerning" =>
          val first  = Integer.parseInt(parts(2)).toChar
          val second = Integer.parseInt(parts(4)).toChar
          val amount = Integer.parseInt(parts(6))
          kernings += ((first, second) -> amount)
        case _ =>
      }
    }
    scanner.close()

    val pages_loaded = pages.map{fn => 
      val image = ImageIO.read(new File(fn))
      val width = image.getWidth
      val height = image.getHeight
      val buffer = new Array[Float](width * height)
      for (y <- 0 until height; x <- 0 until width) {
        val alpha = (image.getRGB(x, y) >> 24) & 0xFF
        val as_float = 1f - alpha / 255f
        buffer(x + y * width) = as_float
      }
      new CharsetSdf(width, height, buffer)
    }
    return new FontData(size, line_height, base, paddings, pages_loaded, chars, kernings)
  }
}

class FontData(
  val size: Int,
  val line_height: Int,
  val base: Int,
  val paddings: Array[Int],
  val pages: Seq[FontRenderer.CharsetSdf],
  val characters: Map[Char, FontRenderer.CharData],
  val kernings: Map[(Char, Char), Int])

class FontRenderer(
  val font_data: FontData,
  val a: Float,
  val b: Float) {

  def calculate_bounds(text: String, size: Float): (Int, Int) = {
    var max_x = 0
    var max_y = 0
    val scaling = size / font_data.size
    var ix = -font_data.paddings(3)
    var iy = -font_data.paddings(0)
    var prev_char = '\u0000'
    for (c <- text) {
      if ('\n' == c) {
        ix = -font_data.paddings(3)
        iy += font_data.line_height - font_data.paddings(0) - font_data.paddings(2)
      } else {
        val char_data = font_data.characters(c)
        ix += font_data.kernings.getOrElse((prev_char, c), 0)
        val tx = ((ix + char_data.tx) * scaling).toInt
        val ty = ((iy + char_data.ty) * scaling).toInt
        val w = (char_data.tw * scaling).toInt
        val h = (char_data.th * scaling).toInt
        max_x = max(max_x, tx + w)
        max_y = max(max_y, ty + h)
        ix += char_data.tw - font_data.paddings(1) - font_data.paddings(3)
      }
      prev_char = c
    }

    return (max_x, max_y)
  }

  def draw(target: RenderTarget, x: Int, y: Int, text: String, size: Float, color: Color): Unit = {
    val scaling = size / font_data.size
    val inverse_scaling = font_data.size / size
    var ix = 0//-font_data.paddings(3)
    var iy = -font_data.base// - font_data.paddings(0)
    var prev_char = '\u0000'
    for (c <- text) {
      if ('\n' == c) {
        ix = -font_data.paddings(3)
        iy += font_data.line_height - font_data.paddings(0) - font_data.paddings(2)
      } else {
        val char_data = font_data.characters(c)
        val page = font_data.pages(char_data.page)
        ix += font_data.kernings.getOrElse((prev_char, c), 0)
        val tx = ((ix + char_data.tx) * scaling).toInt + x
        val ty = ((iy + char_data.ty) * scaling).toInt + y
        val w = (char_data.sw * scaling).toInt
        val h = (char_data.sh * scaling).toInt
        var sy: Float = char_data.sy
        for (py <- ty until ty + h) {
          var sx: Float = char_data.sx
          for (px <- tx until tx + w) {
            val dist = (page.distance_at(sx, sy) - b) * a
            if (dist < 1) {
              val alpha = 1f - max(0f, min(1f, dist))
              target.set_pixel(px, py, color, alpha)
            }
            sx += inverse_scaling
          }
          sy += inverse_scaling
        }
        ix += char_data.tw - font_data.paddings(1) - font_data.paddings(3)
      }
      prev_char = c
    }
  }
}