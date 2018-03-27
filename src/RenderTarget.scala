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

  def fill_circle(center_x: Int, center_y: Int, radius: Int, color: Color, alpha: Float = 1f): Unit = {
    val r_sq = radius * radius
    val r2_sq = (radius + 1) * (radius + 1)
    for (y <- 0 to radius; x  <- 0 to radius) {
      val dist_sq = x * x + y * y
      if (dist_sq < r_sq) {
        set_pixel(center_x + x, center_y + y, color, alpha)
        set_pixel(center_x + x, center_y - y, color, alpha)
        set_pixel(center_x - x, center_y + y, color, alpha)
        set_pixel(center_x - x, center_y - y, color, alpha)
      } else if (dist_sq < r2_sq) {
        val dist = sqrt(dist_sq) - radius
        val coverage = max(0, min(1, 0.5 - dist)).toFloat * alpha
        set_pixel(center_x + x, center_y + y, color, coverage)
        set_pixel(center_x + x, center_y - y, color, coverage)
        set_pixel(center_x - x, center_y + y, color, coverage)
        set_pixel(center_x - x, center_y - y, color, coverage)
      }
    }
  }

  def draw_circle(center_x: Int, center_y: Int, radius: Int, line_width: Int = 1, color: Color, alpha: Float = 1f): Unit = {
    val r1_sq = (radius + 1) * (radius + 1)
    val r2_sq = (radius - line_width) * (radius - line_width)
    for (y <- 0 to radius; x  <- 0 to radius) {
      val dist_sq = x * x + y * y
      if (r2_sq <= dist_sq && dist_sq <= r1_sq) {
        // TODO this can surely be optimized
        val dist = sqrt(dist_sq) - radius
        val coverage1 = min(1, max(0, 0.5 - dist))
        val coverage2 = min(1, max(0, 0.5 - (dist + line_width)))
        val coverage = max(0, min(1, coverage1 - coverage2)).toFloat * alpha
        set_pixel(center_x + x, center_y + y, color, coverage)
        set_pixel(center_x + x, center_y - y, color, coverage)
        set_pixel(center_x - x, center_y + y, color, coverage)
        set_pixel(center_x - x, center_y - y, color, coverage)
      }
    }
  }

  def draw_line(x1: Int, y1: Int, x2: Int, y2: Int, width: Int = 1, color: Color, alpha: Float = 1f): Unit = {
    if (x1 == x2 && y1 == y2) return
    val diff_x = x1 - x2
    val diff_y = y1 - y2
    val normalization = 1.0f / (diff_x * diff_x + diff_y * diff_y)
    val half_width = width / 2
    val half_width_sq = half_width * half_width
    val min_x = min(x1, x2) - half_width - 1
    val max_x = max(x1, x2) + half_width + 1
    val min_y = min(y1, y2) - half_width - 1
    val max_y = max(y1, y2) + half_width + 1
    for (py <- min_y to max_y; px <- min_x to max_x) {
      val x = x1 - px
      val y = y1 - py
      val a = max(0, min(1, (x * diff_x + y * diff_y) * normalization))
      val dist_x = x - diff_x * a
      val dist_y = y - diff_y * a
      val dist_sq = dist_x * dist_x + dist_y * dist_y
      if (dist_sq <= half_width_sq + 1) {
        val dist = sqrt(dist_sq) - half_width
        val coverage = max(0, min(1, 0.5 - dist)).toFloat * alpha
        set_pixel(px, py, color, coverage)
      }
    }
  }

  def commit(): Unit
}