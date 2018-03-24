import ui.Color
import ui.DownScaleFilter
import ui.FontRenderer
import ui.JfxDisplay
import ui.KeyEvent
import ui.MouseEvent

import java.lang.Math._

object Main {

  // Drawing settings
  val multisampling       =   4
  val nodes_columns       =   3
  val nodes_margin        =  70 * multisampling
  val nodes_size          =  50 * multisampling
  val nodes_border_width  =   2 * multisampling
  val line_width          =   2 * multisampling
  val arrow_tip_length    =  10 * multisampling
  val nodes_layout_radius = 150 * multisampling
  val graph_x             = 200 * multisampling
  val graph_y             = 200 * multisampling
  val tree_x              = 450 * multisampling
  val tree_y              =  50 * multisampling

  // Datatype to represent the graph and tree
  case class Node(id: Int)

  // Datatypes for the UI
  case class Button(x1: Int, y1: Int, x2: Int, y2: Int, text: String, enabled: () => Boolean)

  def main(args: Array[String]): Unit = {

    // Set of all Nodes
    val nodes =
      Array(
        Node(id = 0),
        Node(id = 1),
        Node(id = 2),
        Node(id = 3),
        Node(id = 4),
        Node(id = 5),
        Node(id = 6),
        Node(id = 7),
        Node(id = 8))

    // Set of directed edges between the nodes
    val edges =
      Seq[(Node, Seq[Node])](
        (nodes(0), Seq(nodes(1), nodes(3))),
        (nodes(1), Seq(nodes(3), nodes(4))),
        (nodes(2), Seq(nodes(5))),
        (nodes(3), Seq(nodes(6), nodes(7))),
        (nodes(4), Seq(nodes(3), nodes(7))),
        (nodes(5), Seq(nodes(5), nodes(8))),
        (nodes(6), Seq()),
        (nodes(7), Seq(nodes(5), nodes(7))),
        (nodes(8), Seq()))

    var root: Node = null

    // Colors of the nodes
    var node_colors: Map[Node, Color] = null

    // Map just for displaying and interpolating the colors
    var visual_node_colors: Map[Node, Color] = nodes.map(n => n -> Color.White).toMap

    // Stack of nodes to check
    var nodes_todo: Seq[Node] = null

    // Set of tree nodes created by the algorithm.
    var node_parents: Map[Node, Node] = null

    // Useful functions
    def reset(): Unit = {
      if (null == root) root = nodes(0)
      node_colors = nodes.map(n => n -> Color.White).toMap
      nodes_todo = Seq(root)
      node_parents = Map[Node, Node]()
    }

    def select_node(node: Node): Unit = {
      root = node
      reset()
    }
/*
    def get_node_graph_pos(node: Node): (Int, Int) = {
      return (
        (node.id % nodes_columns) * nodes_margin + graph_x,
        (node.id / nodes_columns) * nodes_margin + graph_y)
    }
*/
    def get_node_graph_pos(node: Node): (Int, Int) = {
      val index = nodes.indexOf(node)
      val angle = toRadians(360.0 / nodes.length * index)
      return (
        graph_x + (cos(angle) * nodes_layout_radius).toInt,
        graph_y + (sin(angle) * nodes_layout_radius).toInt)
    }

    def get_node_tree_depth(node: Node): Int = {
      var depth = -1
      var current_node = node
      while (null != current_node) {
        current_node = node_parents.getOrElse(current_node, null)
        depth += 1
      }
      if (0 == depth && node != root)
        return -1
      return depth
    }

    def get_node_tree_pos(node: Node): (Int, Int) = {
      val depth = get_node_tree_depth(node)
      val siblings = nodes.filter(get_node_tree_depth(_) == depth).sortBy(_.id)
      val index = siblings.indexOf(node)

      return (
        index * nodes_margin + tree_x,
        depth * nodes_margin + tree_y)
    }

    // Drawing stuff
    val real_window = new JfxDisplay(800, 600,"👉😎👉 Zoop!")
    val window = new DownScaleFilter(real_window, multisampling, multisampling)
    val font = new FontRenderer(
      "font.png",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz0123456789.,!?'\"-+=/\\%()<>:;_[]{}^µ", 8, 8)

    val step_button = Button(
      x1 = (window.width  * 0.7).toInt,
      y1 = (window.height * 0.9 ).toInt,
      x2 = (window.width  * 0.8).toInt,
      y2 = (window.height * 0.95).toInt,
      text = "Next step",
      enabled = { () => null != nodes_todo && nodes_todo.nonEmpty })

    val reset_button = Button(
      x1 = (window.width  * 0.85).toInt,
      y1 = (window.height * 0.9 ).toInt,
      x2 = (window.width  * 0.95).toInt,
      y2 = (window.height * 0.95).toInt,
      text = "Reset",
      enabled = { () => true })

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
            if (step_button.x1 <= mouse_x && mouse_x <= step_button.x2 &&
                step_button.y1 <= mouse_y && mouse_y <= step_button.y2) {
              step = true
            }
            if (reset_button.x1 <= mouse_x && mouse_x <= reset_button.x2 &&
                reset_button.y1 <= mouse_y && mouse_y <= reset_button.y2) {
              reset()
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

        val arm_x1 = (to_x - normal_x * arrow_tip_length - normal_y * arrow_tip_length / 2).toInt
        val arm_y1 = (to_y - normal_y * arrow_tip_length + normal_x * arrow_tip_length / 2).toInt
        val arm_x2 = (to_x - normal_x * arrow_tip_length + normal_y * arrow_tip_length / 2).toInt
        val arm_y2 = (to_y - normal_y * arrow_tip_length - normal_x * arrow_tip_length / 2).toInt

        window.draw_line(from_x, from_y, to_x, to_y, line_width, color)
        window.draw_line(arm_x1, arm_y1, to_x, to_y, line_width, color)
        window.draw_line(arm_x2, arm_y2, to_x, to_y, line_width, color)
      }

      def draw_node(node: Node, x: Int, y: Int, border_color: Color, fill_color: Color): Unit = {
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
        val text = String.valueOf(node.id)
        val text_x = x / multisampling - (font.char_width * text.length) / 2
        val text_y = y / multisampling - font.char_height / 2
        font.draw(real_window, text_x, text_y, text, border_color)
      }

      def draw_button(button: Button): Unit = {
        window.fill_rect(
          x1 = button.x1,
          y1 = button.y1,
          x2 = button.x2,
          y2 = button.y2,
          color = Color.White)

        window.draw_rect(
          x1 = button.x1,
          y1 = button.y1,
          x2 = button.x2,
          y2 = button.y2,
          line_width = 2 * multisampling,
          color = if (button.enabled()) Color.Dark_Gray else Color.Gray)

        font.draw(
          real_window,
          x = ((button.x1 + button.x2) / multisampling - font.char_width * button.text.length) / 2,
          y = ((button.y1 + button.y2) / multisampling - font.char_height) / 2,
          string = button.text,
          color = if (button.enabled()) Color.Dark_Gray else Color.Gray)
      }

      for ((source, targets) <- edges; target <- targets) {
        if (source == target) {
          val (x1, y1) = get_node_graph_pos(source)
          val nx = (x1 - graph_x).toFloat / nodes_layout_radius
          val ny = (y1 - graph_y).toFloat / nodes_layout_radius

          val x2 = (x1 + nx * nodes_margin / 2 - ny * nodes_margin / 8).toInt
          val y2 = (y1 + ny * nodes_margin / 2 + nx * nodes_margin / 8).toInt
          val x3 = (x1 + nx * nodes_margin / 2 + ny * nodes_margin / 8).toInt
          val y3 = (y1 + ny * nodes_margin / 2 - nx * nodes_margin / 8).toInt
          window.draw_line(x1, y1, x2, y2, line_width, Color.Black)
          window.draw_line(x2, y2, x3, y3, line_width, Color.Black)
          draw_arrow(x3, y3, x1, y1, Color.Black)
        } else {
          val (x1, y1) = get_node_graph_pos(source)
          val (x2, y2) = get_node_graph_pos(target)
          val color = if (nodes_todo.contains(target) && node_parents.getOrElse(target, null) == source) Color.Red else Color.Black
          draw_arrow(x1, y1, x2, y2, color)
        }
      }

      if (null != root) {
        val (center_x, center_y) = get_node_graph_pos(root)
        window.fill_circle(center_x, center_y, nodes_size * 5 / 8, Color.Red)
      }

      for (node <- nodes) {
        val (center_x, center_y) = get_node_graph_pos(node)
        val border_color = if (nodes_todo.contains(node)) Color.Red else Color.Black
        val fill_color = (visual_node_colors(node) + node_colors(node)) * 0.5f
        visual_node_colors += (node -> fill_color)
        draw_node(node, center_x, center_y, border_color, fill_color)
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

      draw_button(step_button)
      draw_button(reset_button)

/*
      val test_angle = toRadians(System.currentTimeMillis() / 1000.0 * 100)
      val test_x1 = window.width / 2
      val test_y1 = window.height / 2
      val test_x2 = test_x1 + (cos(test_angle) * 100 * multisampling).toInt
      val test_y2 = test_y1 + (sin(test_angle) * 100 * multisampling).toInt
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