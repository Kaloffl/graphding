package ui

import java.lang.Math._

trait RenderTarget {

  def width: Int
  def height: Int

  def set_pixel(x: Int, y: Int, color: Color): Unit

  def fill(color: Color): Unit = {
    for (y <- 0 until height; x <- 0 until width) {
      set_pixel(x, y, color)
    }
  }

  def fill_rect(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    val min_x = max(         0, min(x1, x2))
    val min_y = max(         0, min(y1, y2))
    val max_x = min( width - 1, max(x1, x2))
    val max_y = min(height - 1, max(y1, y2))

    for (y <- min_y to max_y; x <- min_x to max_x) {
      set_pixel(x, y, color)
    }
  }

  def draw_rect(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    val min_x = max(         0, min(x1, x2))
    val min_y = max(         0, min(y1, y2))
    val max_x = min( width - 1, max(x1, x2))
    val max_y = min(height - 1, max(y1, y2))

    for (x <- min_x to max_x) {
      set_pixel(x, min_y, color)
      set_pixel(x, max_y, color)
    }

    for (y <- min_y to max_y) {
      set_pixel(min_x, y, color)
      set_pixel(max_x, y, color)
    }
  }

  def draw_line(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    val dx = x1 - x2
    val dy = y1 - y2
    if (abs(dx) < abs(dy)) {
      val sign = if (dy < 0) -1 else 1
      for (y <- 0 to dy by sign) {
        set_pixel(x2 + dx * y / dy, y2 + y, color)
      }
    } else {
      val sign = if (dx < 0) -1 else 1
      for (x <- 0 to dx by sign) {
        set_pixel(x2 + x, y2 + dy * x / dx, color)
      }
    }
  }

  def commit(): Unit
}