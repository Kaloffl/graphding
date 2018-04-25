package ui

import java.util

object Color {
  // LUT of gamma-correct float value for an srgb byte value
  val byte_to_float = new Array[Float](256)
  for (b <- 0 until 256) {
    byte_to_float(b) = Math.pow(b / 255.0, 2.2).toFloat
  }

  // Uses a binary search through the srgb->float LUT to get from float back to a srgb byte
  def float_to_byte(f: Float): Int = {
    var i = util.Arrays.binarySearch(byte_to_float, f)
    if (i < 0) i = -i + 1
    return Math.min(i, 255)
  }

  def from_srgb(r: Int, g: Int, b: Int): Color =
    Color(byte_to_float(r), byte_to_float(g), byte_to_float(b))

  def from_srgb(srgb: Int): Color =
    Color(
      byte_to_float(srgb >> 16 & 0xFF),
      byte_to_float(srgb >>  8 & 0xFF),
      byte_to_float(srgb       & 0xFF))

  def to_srgb(c: Color): Int = {
    val r = float_to_byte(c.r)
    val g = float_to_byte(c.g)
    val b = float_to_byte(c.b)
    return r << 16 | g << 8 | b
  }

  def from_hsb(hue: Float, saturation: Float, brightness: Float): Color = {
    if (saturation == 0) {
      Color(brightness, brightness, brightness)
    } else {
      val h = (hue - Math.floor(hue).toFloat) * 6.0f
      val f = h - Math.floor(h).toFloat
      val p = brightness * (1.0f - saturation)
      val q = brightness * (1.0f - saturation * f)
      val t = brightness * (1.0f - (saturation * (1.0f - f)))
      h.toInt match {
        case 0 ⇒
          Color(brightness, t, p)
        case 1 ⇒
          Color(q, brightness, p)
        case 2 ⇒
          Color(p, brightness, t)
        case 3 ⇒
          Color(p, q, brightness)
        case 4 ⇒
          Color(t, p, brightness)
        case 5 ⇒
          Color(brightness, p, q)
      }
    }
  }

  def lerp(c1: Color, c2: Color, f: Float) =
    Color(
      c1.r * (1 - f) + c2.r * f,
      c1.g * (1 - f) + c2.g * f,
      c1.b * (1 - f) + c2.b * f)

  val Black = Color(0, 0, 0)
  val Red   = Color(1, 0, 0)
  val Green = Color(0, 1, 0)
  val Blue  = Color(0, 0, 1)
  val White = Color(1, 1, 1)

  val Yellow  = Red   + Green
  val Cyan    = Green + Blue
  val Magenta = Blue  + Red

  val Dark_Gray  = White * 0.25f
  val Gray       = White * 0.5f
  val Light_Gray = White * 0.75f
}

case class Color(r: Float, g: Float, b: Float) {
  def sr: Int = Color.float_to_byte(r)
  def sg: Int = Color.float_to_byte(g)
  def sb: Int = Color.float_to_byte(b)

  def +(c: Color) = Color(r + c.r, g + c.g, b + c.b)
  def -(c: Color) = Color(r - c.r, g - c.g, b - c.b)
  def *(c: Color) = Color(r * c.r, g * c.g, b * c.b)
  def *(f: Float) = Color(r * f, g * f, b * f)

  def avg: Float = (r + g + b) / 3
}
