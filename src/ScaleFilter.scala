package ui

class ScaleFilter(
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

  override def commit = target.commit
}