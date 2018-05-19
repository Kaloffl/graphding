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
      val clamped_x = max(0, min(buffer_width  - 1, x))
      val clamped_y = max(0, min(buffer_height - 1, y))
      return values(clamped_x + clamped_y * buffer_width)
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
    var kernings = Map[String, Int]()
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
          kernings += (s"$first$second" -> amount)
        case _ =>
      }
    }
    scanner.close()

    val pages_loaded = pages.map { fn =>
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
  val kernings: Map[String, Int])

class FontRenderer(
  val font_data: FontData) {

  def calculate_bounds(text: String, size: Float): (Int, Int) = {
    var min_x = Int.MaxValue
    var min_y = Int.MaxValue
    var max_x = Int.MinValue
    var max_y = Int.MinValue
    val scaling = size / font_data.size
    val inverse_scaling = font_data.size / size
    var ix = -font_data.characters(text(0)).tx
    var iy = -font_data.base
    for (ci <- 0 until text.length) {
      val c = text.charAt(ci)
      if ('\n' == c) {
        if (ci + 1 < text.length) {
          ix = -font_data.characters(text(ci + 1)).tx
          iy += font_data.line_height - font_data.paddings(0) - font_data.paddings(2)
        }
      } else {
        val char_data = font_data.characters(c)
        val page = font_data.pages(char_data.page)
        if (0 != ci) {
          ix += font_data.kernings.getOrElse(text.substring(ci - 1, ci), 0)
        }

        val pw = (char_data.sw * scaling + 0.5f).toInt
        val ph = (char_data.sh * scaling + 0.5f).toInt

        val start_x = ((ix + char_data.tx) * scaling + 0.5f).toInt
        val start_y = ((iy + char_data.ty) * scaling + 0.5f).toInt

        min_x = min(min_x, start_x)
        min_y = min(min_y, start_y)
        max_x = max(max_x, start_x + pw)
        max_y = max(max_y, start_y + ph)

        ix += char_data.tw - font_data.paddings(1) - font_data.paddings(3)
      }
    }

    return (max_x - min_x, max_y - min_y)
  }

  def draw_approximated(target: RenderTarget, x: Int, y: Int, text: String, size: Float, threshold: Float, color: Color): Unit = {
    val scaling = size / font_data.size
    val inverse_scaling = font_data.size / size
    var ix = -font_data.characters(text(0)).tx
    var iy = -font_data.base
    var dists = new Array[Float](0)
    for (ci <- 0 until text.length) {
      val c = text.charAt(ci)
      if ('\n' == c) {
        if (ci + 1 < text.length) {
          ix = -font_data.characters(text(ci + 1)).tx
          iy += font_data.line_height - font_data.paddings(0) - font_data.paddings(2)
        }
      } else {
        val char_data = font_data.characters(c)
        val page = font_data.pages(char_data.page)
        if (0 != ci) {
          ix += font_data.kernings.getOrElse(text.substring(ci - 1, ci), 0)
        }

        val pw = (char_data.sw * scaling + 0.5f).toInt
        val ph = (char_data.sh * scaling + 0.5f).toInt

        val buffer_rows = pw + 2
        val buffer_cols = ph + 2

        val buffer_size = buffer_rows * buffer_cols
        if (dists.length < buffer_size) {
          dists = new Array[Float](buffer_size)
        }

        java.util.Arrays.fill(dists, 0, buffer_size, Float.MaxValue)
        var di = buffer_rows + 1
        var sy: Float = char_data.sy
        for (py <- 0 until ph) {
          var sx: Float = char_data.sx
          for (px <- 0 until pw) {
            dists(di) = page.distance_at(sx, sy) - threshold
            di += 1
            sx += inverse_scaling
          }
          di += 2
          sy += inverse_scaling
        }

        val start_x = ((ix + char_data.tx) * scaling + 0.5f).toInt + x
        val start_y = ((iy + char_data.ty) * scaling + 0.5f).toInt + y
        val end_x = start_x + pw
        val end_y = start_y + ph
        di = buffer_rows + 1
        for (py <- start_y until end_y) {
          for (px <- start_x until end_x) {
            val dist = dists(di)
            val in = dist <= 0

            val dist_l = dists(di - 1)
            val dist_r = dists(di + 1)
            val dist_t = dists(di - buffer_rows)
            val dist_b = dists(di + buffer_rows)

            val in_l = dist_l <= 0
            val in_r = dist_r <= 0
            val in_t = dist_t <= 0
            val in_b = dist_b <= 0

            if (in == in_l && in == in_r && in == in_t && in == in_b) {
              if (in) target.set_pixel(px, py, color)
            } else {

              val in_tl = dists(di - 1 - buffer_rows) <= 0
              val in_tr = dists(di + 1 - buffer_rows) <= 0
              val in_bl = dists(di - 1 + buffer_rows) <= 0
              val in_br = dists(di + 1 + buffer_rows) <= 0

              def clip(f: Float): Float = if (0 <= f && f <= 0.5f) 0.5f - f else 0f

              var ch = 1f - clip(dist / (dist - dist_t)) - clip(dist / (dist - dist_b))
              var cv = 1f - clip(dist / (dist - dist_l)) - clip(dist / (dist - dist_r))

              val coverage = min(ch, cv)
              if (in) {
                target.set_pixel(px, py, color, coverage)
              } else {
                target.set_pixel(px, py, color, 1.0f - coverage)
              }
            }
            di += 1
          }
          di += 2
        }
        ix += char_data.tw - font_data.paddings(1) - font_data.paddings(3)
      }
    }
  }

  def draw_multisampled(target: RenderTarget, x: Int, y: Int, text: String, size: Float, threshold: Float, color: Color): Unit = {
    val scaling = size / font_data.size
    val inverse_scaling = font_data.size / size
    var ix = -font_data.characters(text(0)).tx
    var iy = -font_data.base
    val multisampling = 4
    val ms_width = 1.0f / multisampling
    val ms_area = ms_width * ms_width
    val mss = ms_width * inverse_scaling
    val mso = (0.5f - multisampling / 2) * ms_width
    for (ci <- 0 until text.length) {
      val c = text.charAt(ci)
      if ('\n' == c) {
        if (ci + 1 < text.length) {
          ix = -font_data.characters(text(ci + 1)).tx
          iy += font_data.line_height - font_data.paddings(0) - font_data.paddings(2)
        }
      } else {
        val char_data = font_data.characters(c)
        val page = font_data.pages(char_data.page)
        if (0 != ci) {
          ix += font_data.kernings.getOrElse(text.substring(ci - 1, ci), 0)
        }

        val pw = (char_data.sw * scaling + 0.5f).toInt
        val ph = (char_data.sh * scaling + 0.5f).toInt

        val start_x = ((ix + char_data.tx) * scaling + 0.5f).toInt + x
        val start_y = ((iy + char_data.ty) * scaling + 0.5f).toInt + y

        for (py <- 0 until ph; px <- 0 until pw) {

          var coverage = 0
          var sy = char_data.sy + (py + mso) * inverse_scaling
          for (my <- 0 until multisampling) {
            var sx = char_data.sx + (px + mso) * inverse_scaling
            for (mx <- 0 until multisampling) {
              if (page.distance_at(sx, sy) <= threshold) {
                coverage += 1
              }
              sx += mss
            }
            sy += mss
          }
          target.set_pixel(start_x + px, start_y + py, color, coverage * ms_area)
        }
        ix += char_data.tw - font_data.paddings(1) - font_data.paddings(3)
      }
    }
  }

  def draw(target: RenderTarget, x: Int, y: Int, text: String, size: Float, threshold: Float, color: Color): Unit = {
    if (size < 20) {
      draw_multisampled(target, x, y, text, size, threshold, color)
    } else {
      draw_approximated(target, x, y, text, size, threshold, color)
    }
  }

}
