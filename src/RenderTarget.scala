package ui

import java.lang.Math._

trait RenderTarget {

  def width: Int
  def height: Int

  def set_pixel(x: Int, y: Int, color: Color, alpha: Float = 1f): Unit

  def fill(color: Color, alpha: Float = 1f): Unit = {
    for (y <- 0 until height; x <- 0 until width) {
      set_pixel(x, y, color, alpha)
    }
  }

  def fill_rect(x1: Int, y1: Int, x2: Int, y2: Int, color: Color, alpha: Float = 1f): Unit = {
    val min_x = max(         0, min(x1, x2))
    val min_y = max(         0, min(y1, y2))
    val max_x = min( width - 1, max(x1, x2))
    val max_y = min(height - 1, max(y1, y2))

    for (y <- min_y to max_y; x <- min_x to max_x) {
      set_pixel(x, y, color, alpha)
    }
  }

  def draw_rect(x1: Int, y1: Int, x2: Int, y2: Int, line_width: Int = 1, color: Color, alpha: Float = 1f): Unit = {
    val min_x = max(         0, min(x1, x2))
    val min_y = max(         0, min(y1, y2))
    val max_x = min( width - 1, max(x1, x2))
    val max_y = min(height - 1, max(y1, y2))

    for (y <- 0 until line_width; x <- min_x to max_x) {
      set_pixel(x, min_y + y, color, alpha)
      set_pixel(x, max_y - y, color, alpha)
    }

    for (y <- min_y to max_y; x <- 0 until line_width) {
      set_pixel(min_x + x, y, color, alpha)
      set_pixel(max_x - x, y, color, alpha)
    }
  }

  def commit(): Unit
}
