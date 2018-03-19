import ui.Color
import ui.DownScaleFilter
import ui.FontRenderer
import ui.JfxDisplay
import ui.KeyEvent
import ui.MouseEvent

import java.lang.Math._

object Main {

  // Drawing settings
  val multisampling      =   4
  val nodes_columns      =   3
  val nodes_margin       = 100 * multisampling
  val nodes_size         =  50 * multisampling
  val nodes_border_width =   2 * multisampling
  val line_width         =   1 * multisampling
  val arrow_tip_length   =  10 * multisampling
  val graph_x            =  50 * multisampling
  val graph_y            =  50 * multisampling
  val tree_x             = 450 * multisampling
  val tree_y             =  50 * multisampling

  // Datatypes to represent the graph and tree
  case class Node(id: Int)

  def main(args: Array[String]): Unit = {

    // Set of all Nodes
    val nodes =
      Array(
        Node(id = 0),
        Node(id = 1),
        Node(id = 2),
        Node(id = 3),
        Node(id = 4),
        Node(id = 5))

    // Set of directed edges between the nodes
    val edges =
      Seq[(Node, Seq[Node])](
        (nodes(0), Seq(nodes(1), nodes(3))),
        (nodes(1), Seq(nodes(3), nodes(4))),
        (nodes(2), Seq(nodes(4), nodes(5))),
        (nodes(3), Seq()),
        (nodes(4), Seq(nodes(3))),
        (nodes(5), Seq(nodes(5))))

    var root: Node = null

    // Colors of the nodes
    var node_colors: Map[Node, Color] = null

    // Stack of nodes to check
    var nodes_todo: Seq[Node] = null

    // Set of tree nodes created by the algorithm.
    var node_parents: Map[Node, Node] = null

    // Useful functions
    def reset(): Unit = {
      root = nodes(0)
      node_colors = nodes.map(n => n -> Color.White).toMap
      nodes_todo = Seq(nodes(0))
      node_parents = Map[Node, Node]()
    }

    def select_node(node: Node): Unit = {
      reset()
      root = node
      nodes_todo = Seq(node)
    }

    def get_node_graph_pos(node: Node): (Int, Int) = {
      return (
        (node.id % nodes_columns) * nodes_margin + graph_x,
        (node.id / nodes_columns) * nodes_margin + graph_y)
    }

    def get_node_tree_pos(node: Node): (Int, Int) = {
      return (
        (node.id % nodes_columns) * nodes_margin + tree_x,
        (node.id / nodes_columns) * nodes_margin + tree_y)
    }

    // Drawing stuff
    val real_window = new JfxDisplay(800, 600,"👉😎👉 Zoop!")
    val window = new DownScaleFilter(real_window, multisampling, multisampling)
/* TODO: make a readable font and put it in the project
    val font = new FontRenderer(
      "C:/dev/font2.png",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz0123456789.,!?'\"-+=/\\%()<>:;_[]{}^µ", 8, 8)
*/

    reset()
    var mouse_x = 0
    var mouse_y = 0

    while (true) {
      val start_time = System.nanoTime()

      // Handle User Inputs
      var step = false
      for (event <- real_window.events) {
        event match {
          case KeyEvent(KeyEvent.Key_Space, true) => 
            step = true
          case KeyEvent(KeyEvent.Mouse_1, true) =>
            for (node <- nodes) {
              val (node_x, node_y) = get_node_graph_pos(node)
              val dist_x = node_x - mouse_x
              val dist_y = node_y - mouse_y
              if (dist_x * dist_x + dist_y * dist_y < nodes_size * nodes_size) {
                select_node(node)
              }
            }
          case MouseEvent(x, y, _) =>
            mouse_x = x * multisampling
            mouse_y = y * multisampling
          case _ =>
        }
      }

      // Run Algorithm
      if (step) {

        // If there are still nodes on the stack
        if (nodes_todo.nonEmpty) {

          // take the top one
          val current_node = nodes_todo.head

          // set its color to gray
          node_colors += (current_node -> Color.Gray)

          // go through the adjacency list to find the neighbors of the current node
          for ((source, targets) <- edges) {
            if (source == current_node) {

              // check if any neighbors are still colored white
              targets.find(node_colors(_) == Color.White) match {
                case Some(target) =>
                  // If a white neighbor is found, it gets pushed on the stack and
                  // its parent is set to the current node.
                  node_parents += (target -> source)
                  nodes_todo = target +: nodes_todo
                case None =>
                  // If no white neighbor is found, this node is finished. It will be
                  // colored black and removed from the stack.
                  node_colors += (current_node -> Color.Black)
                  nodes_todo = nodes_todo.tail
              }
            }
          }
        }
      }

      // Rendering
      window.fill(Color.White)

      def draw_arrow(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
        val diff_x = x2 - x1
        val diff_y = y2 - y1
        val inverse_length = 1 / sqrt(diff_x * diff_x + diff_y * diff_y)
        val normal_x = diff_x * inverse_length
        val normal_y = diff_y * inverse_length
        val from_x = x1
        val from_y = y1
        val to_x = (x2 - normal_x * nodes_size / 2).toInt
        val to_y = (y2 - normal_y * nodes_size / 2).toInt

        val arm_x1 = (to_x - normal_x * arrow_tip_length - normal_y * arrow_tip_length).toInt
        val arm_y1 = (to_y - normal_y * arrow_tip_length + normal_x * arrow_tip_length).toInt
        val arm_x2 = (to_x - normal_x * arrow_tip_length + normal_y * arrow_tip_length).toInt
        val arm_y2 = (to_y - normal_y * arrow_tip_length - normal_x * arrow_tip_length).toInt

        window.draw_line(from_x, from_y, to_x, to_y, line_width, color)
        window.draw_line(arm_x1, arm_y1, to_x, to_y, line_width, color)
        window.draw_line(arm_x2, arm_y2, to_x, to_y, line_width, color)
      }

      def draw_node(node: Node, x: Int, y: Int, border_color: Color, fill_color: Color): Unit = {
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
      }

      for ((source, targets) <- edges; target <- targets) {
        if (source == target) {
          val (x1, y1) = get_node_graph_pos(source)
          val x2 = x1 + nodes_margin / 2
          val y2 = y1 - nodes_size / 2
          val x3 = x2
          val y3 = y1 + nodes_size / 2
          val x4 = x1
          val y4 = y1
          window.draw_line(x1, y1, x2, y2, line_width, Color.Black)
          window.draw_line(x2, y2, x3, y3, line_width, Color.Black)
          draw_arrow(x3, y3, x4, y4, Color.Black)
        } else {
          val (x1, y1) = get_node_graph_pos(source)
          val (x2, y2) = get_node_graph_pos(target)
          draw_arrow(x1, y1, x2, y2, Color.Black)
        }
      }

      for (node <- nodes) {
        val (center_x, center_y) = get_node_graph_pos(node)
        val border_color = if (nodes_todo.contains(node)) Color.Red else Color.Black
        draw_node(node, center_x, center_y, border_color, node_colors(node))
      }

      for ((target, source) <- node_parents) {
        val (x1, y1) = get_node_tree_pos(source)
        val (x2, y2) = get_node_tree_pos(target)
        draw_arrow(x1, y1, x2, y2, Color.Black)
      }

      for (node <- nodes) {
        if (node == root || node_parents.contains(node)) {
        val (center_x, center_y) = get_node_tree_pos(node)
        draw_node(node, center_x, center_y, Color.Black, Color.White)
        }
      }


/*
      val test_angle = toRadians(System.currentTimeMillis() / 1000.0 * 100)
      val test_x1 = window.width / 2
      val test_y1 = window.height / 2
      val test_x2 = test_x1 + (cos(test_angle) * 100).toInt
      val test_y2 = test_y1 + (sin(test_angle) * 100).toInt
      draw_arrow(test_x1, test_y1, test_x2, test_y2, Color.Black)
*/

      // Wait for Frame time and draw to screen
      val end_time = System.nanoTime()
      val dur_ms = Math.ceil((end_time - start_time) / 1000000).toInt
      if (dur_ms < 30) Thread.sleep(33 - dur_ms)

      window.commit()
    }
  }
}