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

  def draw_rect(x1: Int, y1: Int, x2: Int, y2: Int, line_width: Int = 1, color: Color): Unit = {
    val min_x = max(         0, min(x1, x2))
    val min_y = max(         0, min(y1, y2))
    val max_x = min( width - 1, max(x1, x2))
    val max_y = min(height - 1, max(y1, y2))

    for (y <- 0 until line_width; x <- min_x to max_x) {
      set_pixel(x, min_y + y, color)
      set_pixel(x, max_y - y, color)
    }

    for (y <- min_y to max_y; x <- 0 until line_width) {
      set_pixel(min_x + x, y, color)
      set_pixel(max_x - x, y, color)
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
    val diff_x = x1 - x2
    val diff_y = y1 - y2
    val normalization = 1.0f / (diff_x * diff_x + diff_y * diff_y)
    val max_dist_sq = width * width / 4f
    val min_x = min(x1, x2) - width
    val max_x = max(x1, x2) + width
    val min_y = min(y1, y2) - width
    val max_y = max(y1, y2) + width
    for (py <- min_y to max_y; px <- min_x to max_x) {
      val x = x1 - px
      val y = y1 - py
      val a = max(0, min(1, (x * diff_x + y * diff_y) * normalization))
      val dist_x = x - diff_x * a
      val dist_y = y - diff_y * a
      if (dist_x * dist_x + dist_y * dist_y <= max_dist_sq) {
        set_pixel(px, py, color)
      }
    }
  }

  def commit(): Unit
}