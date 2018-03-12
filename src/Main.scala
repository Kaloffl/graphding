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
      window.fill(Color.White)

      
      
      window.commit()
    }
  }
}