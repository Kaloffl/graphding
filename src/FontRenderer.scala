package ui

import java.io.File
import javax.imageio.ImageIO

class FontRenderer(font_file: String, val alphabet: String, val char_width: Int, val char_height: Int) {

  val image = ImageIO.read(new File(font_file))

  def draw(target: RenderTarget, x: Int, y: Int, char: Char, color: Color): Unit = {
    val index = alphabet.indexOf(char)
    if (-1 == index) throw new IllegalArgumentException(s"Char '$char' not in alphabet $alphabet")
    val cx = (index * char_width) % image.getWidth
    val cy = ((index * char_width) / image.getWidth) * char_height
    for (oy <- 0 until char_height; ox <- 0 until char_width) {
      val argb = image.getRGB(cx + ox, cy + oy)
      if (0 != (argb & 0xFF000000)) {
        target.set_pixel(x + ox, y + oy, Color.from_srgb(argb) * color)
      }
    }
  }

  def draw(target: RenderTarget, x: Int, y: Int, string: String, color: Color): Unit = {
    for (i <- 0 until string.length) {
      draw(target, x + i * char_width, y, string.charAt(i), color)
    }
  }
}
