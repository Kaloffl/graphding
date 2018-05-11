package ui

import java.lang.Math._

object SdfRenderer {
  type Sdf = (Float, Float) => Float

  //
  // BASIC SHAPES
  //

  // SDF of the unit circle centered at the origin.
  def circle(x: Float, y: Float): Float = sqrt(x * x + y * y).toFloat - 1f

  // SDF of a square centered at the origin with side length of 2.
  def square(x: Float, y: Float): Float = max(abs(x), abs(y)) - 1f

  // SDF that splits space in half. Positive x and 0 is inside, negative x is outside.
  def half(x: Float, y: Float): Float = x

  //
  // PARAMETERIZED SHAPES
  //

  def circle(center_x: Float, center_y: Float, radius: Float): Sdf = (x, y) => {
    val x2 = x - center_x
    val y2 = y - center_y
    sqrt(x2 * x2 + y2 * y2).toFloat - radius
  }

  def ring(center_x: Float, center_y: Float, inner_radius: Float, outer_radius: Float): Sdf = (x, y) => {
    val x2 = x - center_x
    val y2 = y - center_y
    val dist = sqrt(x2 * x2 + y2 * y2).toFloat
    max(dist - outer_radius, -(dist - inner_radius))
  }

  def line(x1: Float, y1: Float, x2: Float, y2: Float, width: Float): Sdf = {
    val dx = x2 - x1
    val dy = y2 - y1
    val inverse_length_squared = 1f  / (dx * dx + dy * dy)
    val nx = dx * inverse_length_squared
    val ny = dy * inverse_length_squared
    val half_width = width / 2
    (x, y) => {
      val x3 = x - x1
      val y3 = y - y1
      val a = max(0, min(1, x3 * nx + y3 * ny))
      val dist_x = x3 - dx * a
      val dist_y = y3 - dy * a
      sqrt(dist_x * dist_x + dist_y * dist_y).toFloat - half_width
    }
  }

  def half(nx: Float, ny: Float, d: Float): Sdf = (x, y) => {
    // nx * nx + ny * ny must be 1
    x * nx + y * ny - d
  }

  def triangle(x1: Float, y1: Float, x2: Float, y2: Float, x3: Float, y3: Float): Sdf = {
    // Calculate the deltas for the three sides.
    val dx21 = x2 - x1
    val dy21 = y2 - y1
    val dx32 = x3 - x2
    val dy32 = y3 - y2
    val dx13 = x1 - x3
    val dy13 = y1 - y3

    // Get a sign which way points "outside" on each side.
    // This is necessary to make the inputs winding independent.
    val s21 = if (dx21 * dy32 < dy21 * dx32) 1 else -1
    val s32 = if (dx32 * dy13 < dy32 * dx13) 1 else -1
    val s13 = if (dx13 * dy21 < dy13 * dx21) 1 else -1

    // Calculate the inverse length and normal vectors that point away from the triangle on each side.
    val il21 = s21 / sqrt(dx21 * dx21 + dy21 * dy21)
    val nx21 = (-dy21 * il21).toFloat
    val ny21 = ( dx21 * il21).toFloat

    val il32 = s32 / sqrt(dx32 * dx32 + dy32 * dy32)
    val nx32 = (-dy32 * il32).toFloat
    val ny32 = ( dx32 * il32).toFloat

    val il13 = s13 / sqrt(dx13 * dx13 + dy13 * dy13)
    val nx13 = (-dy13 * il13).toFloat
    val ny13 = ( dx13 * il13).toFloat

    // Calculate the distance from the origin to the line going through each vertex pair.
    val d21 = x1 * nx21 + y1 * ny21
    val d32 = x2 * nx32 + y2 * ny32
    val d13 = x3 * nx13 + y3 * ny13

    // Finally return the SDF that takes any point and returns the distance to the triangle.
    (x, y) => {
      max(max(
        x * nx21 + y * ny21 - d21,
        x * nx32 + y * ny32 - d32),
        x * nx13 + y * ny13 - d13)
    }
  }

  //
  // SHAPE MANIPULATION
  //

  // takes a SDF and returns a SDF with the given translation applied
  def translate(sdf: Sdf, translate_x: Float, translate_y: Float): Sdf = (x, y) => sdf(x + translate_x, y + translate_y)

  // takes a SDF and returns a SDF with the given scaling applied
  def scale(sdf: Sdf, scale_x: Float, scale_y: Float): Sdf = (x, y) => sdf(x * scale_x, y * scale_y)

  // takes a SDF and returns one rotatet clockwise by the given angle around the given pivot
  def rotate_cw(sdf: Sdf, radians: Float, pivot_x: Float = 0f, pivot_y: Float = 0f): Sdf = {
    val c = cos(radians).toFloat
    val s = sin(radians).toFloat
    (x, y) => {
      // TODO test direction
      val x2 = x - pivot_x
      val y2 = y - pivot_y
      sdf(x2 * c - y2 * s + pivot_x, x2 * s + y2 * c + pivot_y)
    }
  }

  // takes a SDF and returns one rotatet counter-clockwise by the given angle around the given pivot
  def rotate_ccw(sdf: Sdf, radians: Float, pivot_x: Float = 0f, pivot_y: Float = 0f): Sdf = {
    val c = cos(radians).toFloat
    val s = sin(radians).toFloat
    (x, y) => {
      // TODO test direction
      val x2 = x - pivot_x
      val y2 = y - pivot_y
      sdf(x2 * c - y2 * s + pivot_x, x2 * s + y2 * c + pivot_y)
    }
  }

  //
  // SHAPE COMBINATION
  //

  // takes two SDFs and returns one where both are conbined
  def unite(sdf1: Sdf, sdf2: Sdf): Sdf = (x, y) => min(sdf1(x, y), sdf2(x, y))

  // takes two SDFs and returns only the overlap
  def intersect(sdf1: Sdf, sdf2: Sdf): Sdf = (x, y) => max(sdf1(x, y), sdf2(x, y))

  // takes two SDFs and returns one where the area of the second one is removed from the first
  def subtract(sdf1: Sdf, sdf2: Sdf): Sdf = (x, y) => max(sdf1(x, y), -sdf2(x, y))

  // takes a SDF and returns one where inside and outside are swapped
  def invert(sdf: Sdf): Sdf = (x, y) => -sdf(x, y)

  //
  // SHAPE RENDERING
  //

  private val half_sqrt_2 = sqrt(2).toFloat * 0.5f
  private val inverse_sqrt_2 = (1.0 / sqrt(2)).toFloat

  def coverage(
    left: Float, top: Float,
    size: Float,
    max_depth: Int,
    sdf: Sdf
  ): Float = {
    val half_size = size * 0.5f
    val center_x = left + half_size
    val center_y = top  + half_size

    val dist = sdf(center_x, center_y)

    if (0 == max_depth) {
      if (dist <= 0) return size * size
      return 0.0f
    }

    val half_diag = size * half_sqrt_2

    if ( dist >= half_diag) return 0.0f
    if (-dist >= half_diag) return size * size
    return coverage(    left,      top, half_size, max_depth - 1, sdf) +
           coverage(center_x,      top, half_size, max_depth - 1, sdf) +
           coverage(    left, center_y, half_size, max_depth - 1, sdf) +
           coverage(center_x, center_y, half_size, max_depth - 1, sdf)
  }

  def render(
    target: RenderTarget,
    left: Int, top: Int,
    width: Int, height: Int,
    sdf: Sdf,
    color: Color,
    alpha: Float
  ): Unit = {

    if (width <= 3 && height <= 3) {
      for (y <- 0 until height; x <- 0 until width) {
        val c = coverage(left - 0.5f + x, top - 0.5f + y, 1.0f, 4, sdf)
        target.set_pixel(left + x, top + y, color, c * alpha)
      }
    } else {
      val half_width = width * 0.5f
      val half_height = height * 0.5f
      val center_x = left + half_width - 0.5f
      val center_y = top + half_height - 0.5f

      val dist = sdf(center_x, center_y)
      val abs_dist = abs(dist)
      val dist_squared = dist * abs_dist
      val half_diag_squared = (half_width * half_width + half_height * half_height)


      if (dist_squared < half_diag_squared) {
        if (-dist_squared >= half_diag_squared) {
          target.fill_rect(left, top, left + width - 1, top + height - 1, color, alpha)
        } else {
          if (half_width < abs_dist || half_height < abs_dist) {
            if (height < width) {
              val    left_width = ceil(half_width - sqrt(dist * dist - half_height * half_height)).toInt
              val  center_width = max(0, width - 2 * left_width)
              val   right_width = width - left_width - center_width
              val      center_x = left + left_width
              val         right = center_x + center_width

              render(target,  left, top,  left_width, height, sdf, color, alpha)
              render(target, right, top, right_width, height, sdf, color, alpha)

              if (dist < 0 && 0 < center_width) {
                target.fill_rect(center_x, top, right - 1, height + top - 1, color, alpha)
              }
            } else {
              val    top_height = ceil(half_height - sqrt(dist * dist - half_width * half_width)).toInt
              val center_height = max(0, height - 2 * top_height)
              val bottom_height = height - top_height - center_height
              val      center_y = top  + top_height
              val        bottom = center_y + center_height

              render(target, left,    top, width,    top_height, sdf, color, alpha)
              render(target, left, bottom, width, bottom_height, sdf, color, alpha)

              if (dist < 0 && 0 < center_height) {
                target.fill_rect(left, center_y, left + width - 1, bottom - 1, color, alpha)
              }
            }
          } else {
            val half_inner_size = abs_dist * inverse_sqrt_2

            val    left_width = ceil(half_width - half_inner_size).toInt
            val  center_width = max(0, width - 2 * left_width)
            val   right_width = width - left_width - center_width
            val      center_x = left + left_width
            val         right = center_x + center_width

            val    top_height = ceil(half_height - half_inner_size).toInt
            val center_height = max(0, height - 2 * top_height)
            val bottom_height = height - top_height - center_height
            val      center_y = top  + top_height
            val        bottom = center_y + center_height

            if (height < width) {
              render(target,  left, top,  left_width, height, sdf, color, alpha)
              render(target, right, top, right_width, height, sdf, color, alpha)

              if (0 < center_width && 0 < top_height) {
                render(target, center_x,    top, center_width,    top_height, sdf, color, alpha)
                render(target, center_x, bottom, center_width, bottom_height, sdf, color, alpha)
              }
            } else {
              render(target, left,    top, width,    top_height, sdf, color, alpha)
              render(target, left, bottom, width, bottom_height, sdf, color, alpha)

              if (0 < center_height && 0 < left_width) {
                render(target,  left, center_y,  left_width, center_height, sdf, color, alpha)
                render(target, right, center_y, right_width, center_height, sdf, color, alpha)
              }
            }

            if (dist < 0 && 0 < center_width && 0 < center_height) {
              target.fill_rect(center_x, center_y, right - 1, bottom - 1, color, alpha)
            }
          }
        }
      }
      //target.draw_rect(left, top, left + width, top + height, 1, Color.Red, 0.5f)
    }
  }

  def render(
    target: RenderTarget,
    sdf: Sdf,
    color: Color,
    alpha: Float = 1f
  ): Unit = {
    render(target, 0, 0, target.width, target.height, sdf, color, alpha)
  }
}
