import ui.JfxDisplay
import ui.Color

object Main {
  
  def main(args: Array[String]): Unit = {

    case class Node(id: Int)
    case class Edge(from: Node, to: Node)

    val nodes =
      Array(
        Node(id = 0),
        Node(id = 1),
        Node(id = 2),
        Node(id = 3),
        Node(id = 4),
        Node(id = 5))

    val edges =
      Array(
        Edge(from = nodes(0), to = nodes(1)),
        Edge(from = nodes(0), to = nodes(3)),
        Edge(from = nodes(1), to = nodes(3)),
        Edge(from = nodes(1), to = nodes(4)),
        Edge(from = nodes(2), to = nodes(4)),
        Edge(from = nodes(2), to = nodes(5)),
        Edge(from = nodes(4), to = nodes(3)),
        Edge(from = nodes(5), to = nodes(5)))

    val window = new JfxDisplay(800, 600)

    while (true) {
      val start_time = System.nanoTime()

      window.fill(Color.White)

      val a = 2 * Math.PI * System.currentTimeMillis() / 10000
      val x = Math.cos(a)
      val y = Math.sin(a)

      val x1 = window.width / 2
      val y1 = window.height / 2

      val x2 = x1 + (x * 100).toInt
      val y2 = y1 + (y * 100).toInt

      window.draw_line(x1, y1, x2, y2, Color.Black)

      window.draw_rect(x2 - 5, y2 - 5, x2 + 5, y2 + 5, Color.Blue)
      
      val end_time = System.nanoTime()
      val dur_ms = Math.ceil((end_time - start_time) / 1000000).toInt
      if (dur_ms < 30) Thread.sleep(33 - dur_ms)

      window.commit()
    }
  }
}