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
    val s21 = if (dx21 * dy32 < dy21 * dx32)  1 else -1
    val s32 = if (dx32 * dy13 < dy32 * dx13)  1 else -1
    val s13 = if (dx13 * dy21 < dy13 * dx21)  1 else -1
    
    // Calculate the inverse length and normal vectors that point away from the triangle on each side.
    val il21 = 1.0 / sqrt(dx21 * dx21 + dy21 * dy21)
    val nx21 = (-dy21 * il21).toFloat * s21
    val ny21 = ( dx21 * il21).toFloat * s21

    val il32 = 1.0 / sqrt(dx32 * dx32 + dy32 * dy32)
    val nx32 = (-dy32 * il32).toFloat * s32
    val ny32 = ( dx32 * il32).toFloat * s32

    val il13 = 1.0 / sqrt(dx13 * dx13 + dy13 * dy13)
    val nx13 = (-dy13 * il13).toFloat * s13
    val ny13 = ( dx13 * il13).toFloat * s13

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

  private val half_sqrt_2 = sqrt(2) / 2

  def render(
    target: RenderTarget,
    sdf: Sdf,
    color: Color,
    alpha: Float = 1f,
    multisampling: Int = 16
  ): Unit = {

    val d1 = sdf(           0,             0)
    val d2 = sdf(           0, target.height)
    val d3 = sdf(target.width,             0)
    val d4 = sdf(target.width, target.height)

    val screen_diag_square = target.width * target.width + target.height * target.height

    if (d1 * abs(d1) > screen_diag_square ||
        d2 * abs(d2) > screen_diag_square ||
        d3 * abs(d3) > screen_diag_square ||
        d4 * abs(d4) > screen_diag_square) {
      return
    }

    val det1 = (-target.height + d2 - d1) * (-target.height - d2 + d1) * (-target.height + d2 + d1) * (target.height + d2 + d1)
    val det2 = (-target.height + d4 - d3) * (-target.height - d4 + d3) * (-target.height + d4 + d3) * (target.height + d4 + d3)
    val det3 = (-target.width + d3 - d1) * (-target.width - d3 + d1) * (-target.width + d3 + d1) * (target.width + d3 + d1)
    val det4 = (-target.width + d4 - d2) * (-target.width - d4 + d2) * (-target.width + d4 + d2) * (target.width + d4 + d2)

    val min_x = if (d1 > 0 && d2 > 0 && det1 > 0) floor(               sqrt(det1) / target.height / 2).toInt else 0
    val max_x = if (d3 > 0 && d4 > 0 && det2 > 0) ceil( target.width - sqrt(det2) / target.height / 2).toInt else target.width
    val min_y = if (d1 > 0 && d3 > 0 && det3 > 0) floor(               sqrt(det3) / target.width  / 2).toInt else 0
    val max_y = if (d2 > 0 && d4 > 0 && det4 > 0) ceil(target.height - sqrt(det4) / target.width  / 2).toInt else target.height

    //target.draw_rect(min_x, min_y, max_x, max_y, 1, Color.Red)

    val half_ms = multisampling / 2
    val inverse_square_ms = 1f / (multisampling * multisampling)

    var pixel_y = min_y
    while (pixel_y < max_y) {

      var pixel_x = min_x
      while (pixel_x < max_x) {
        val dist = sdf(pixel_x, pixel_y)

        if (half_sqrt_2 <= dist) {
          pixel_x += max(1, dist.toInt)
          //target.set_pixel(pixel_x, pixel_y, Color.Green, 0.5f)
        } else {
          if (dist <= -half_sqrt_2) {
            val end_range = pixel_x + max(1, (-dist).toInt)
            while (pixel_x < end_range) {
              target.set_pixel(pixel_x, pixel_y, color, alpha)
              pixel_x += 1
            }
          } else {
            var coverage = 0f
            var multisampling_y = -half_ms
            while (multisampling_y < half_ms) {
              val subpixel_y = pixel_y + (multisampling_y + 0.5f) / multisampling
              var multisampling_x = -half_ms
              while (multisampling_x < half_ms) {
                val subpixel_x = pixel_x + (multisampling_x + 0.5f) / multisampling

                val dist = sdf(subpixel_x, subpixel_y)
                if (dist <= 0) {
                  val range = max(1, min(half_ms - multisampling_x, (-dist * multisampling).toInt))
                  coverage += range * inverse_square_ms
                  multisampling_x += range
                } else {
                  multisampling_x += max(1, (dist * multisampling).toInt)
                }
              }
              multisampling_y += 1
            }
            target.set_pixel(pixel_x, pixel_y, color, alpha * coverage)
            pixel_x += 1
          }
        }
      }

      pixel_y += 1
    }
  }
}
