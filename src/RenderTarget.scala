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

  def fill_circle(center_x: Int, center_y: Int, radius: Int, color: Color): Unit = {
    val r2 = radius * radius
    for (y <- 0 to radius; x  <- 0 to radius) {
      if (x * x + y * y <= r2) {
        set_pixel(center_x + x, center_y + y, color)
        set_pixel(center_x + x, center_y - y, color)
        set_pixel(center_x - x, center_y + y, color)
        set_pixel(center_x - x, center_y - y, color)
      }
    }
  }

  def draw_circle(center_x: Int, center_y: Int, radius: Int, color: Color): Unit = {
    for (y <- 0 to radius; x  <- 0 to radius) {
      val d = sqrt(x * x + y * y) - radius
      if (-1 < d && d < 1) {
        set_pixel(center_x + x, center_y + y, color)
        set_pixel(center_x + x, center_y - y, color)
        set_pixel(center_x - x, center_y + y, color)
        set_pixel(center_x - x, center_y - y, color)
      }
    }
  }

  def draw_line(x1: Int, y1: Int, x2: Int, y2: Int, width: Int, color: Color): Unit = {
    if (x1 == x2 && y1 == y2) return
    val dx = x1 - x2
    val dy = y1 - y2
    val min_x = min(x1, x2) - width
    val max_x = max(x1, x2) + width
    val min_y = min(y1, y2) - width
    val max_y = max(y1, y2) + width
    for (py <- min_y to max_y; px <- min_x to max_x) {
      val x = x1 - px
      val y = y1 - py
      val a = max(0, min(1, (x * dx + y * dy) / (dx * dx + dy * dy).toFloat))
      val b = x - dx * a
      val c = y - dy * a
      if (b * b + c * c <= width * width / 4f) {
        set_pixel(px, py, color)
      }
    }
  }
/*
  def line(x: Double, y: Double, x1: Double, y1: Double, x2: Double, y2: Double, width: Double): Float = {
    val dx = x1 - x2
    val dy = y1 - y2
    val xx = x1 - x
    val yy = y1 - y
    val a = max(0, min(1, (xx * dx + yy * dy) / (dx * dx + dy * dy)))
    val b = xx - dx * a
    val c = yy - dy * a
    return (sqrt(b * b + c * c) - width / 2).toFloat
  }
*/

/*
  def draw_line(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    val dx = x1 - x2
    val dy = y1 - y2
    if (0 == dx) {
      val sign = if (dy < 0) -1 else 1
      for (y <- 0 to dy by sign) {
       set_pixel(x1, y2 + y, color)
      }
    } else if (0 == dy) {
      val sign = if (dx < 0) -1 else 1
      for (x <- 0 to dx by sign) {
        set_pixel(x2 + x, y1, color)
      }
    } else if (abs(dx) < abs(dy)) {
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
*/

  def commit(): Unit
}