package ui

import java.util

class UpScaleFilter(
    val target: RenderTarget,
    val pixel_width: Int,
    val pixel_height: Int) extends RenderTarget {

  override def width: Int = target.width / pixel_width
  override def height: Int = target.height / pixel_height

  override def set_pixel(x: Int, y: Int, color: Color): Unit = {
    val min_x = x * pixel_width
    val min_y = y * pixel_height
    val max_x = min_x + pixel_width
    val max_y = min_y + pixel_height

    for(x <- min_x until max_x; y <- min_y until max_y) {
      target.set_pixel(x, y, color)
    }
  }

  override def commit(): Unit = target.commit()
}

class DownScaleFilter(
    val target: RenderTarget,
    val multisample_h: Int,
    val multisample_v: Int) extends RenderTarget {

  override def width: Int = target.width * multisample_h
  override def height: Int = target.height * multisample_v

  val multisample_buffer = new Array[Color](width * height)
  val downscaled_buffer = new Array[Color](target.width * target.height)
  val scale_factor = (1.0f / multisample_h / multisample_v)

  fill(Color.White)

  override def set_pixel(x: Int, y: Int, color: Color): Unit = {
    if (0 <= x && x < width && 0 <= y && y < height) {
      val old_color = multisample_buffer(x + y * width)
      multisample_buffer(x + y * width) = color

      val sx = x / multisample_h
      val sy = y / multisample_v
      val index = sx + sy * target.width
      downscaled_buffer(index) += (color - old_color) * scale_factor
      
      target.set_pixel(sx, sy, downscaled_buffer(index))
    }
  }

  override def fill(color: Color): Unit = {
    util.Arrays.fill(multisample_buffer.asInstanceOf[Array[Object]], color)
    util.Arrays.fill(downscaled_buffer.asInstanceOf[Array[Object]], color)
    target.fill(color)
  }

  override def commit(): Unit = target.commit()
}