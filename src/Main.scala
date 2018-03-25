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
  case class Button(
    bounds: Rectangle,
    text: String,
    enabled: () => Boolean,
    action : () => Unit)


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

    // Node that is currently selected as the root node of the search
    var root: Node = null

    // Colors of the nodes
    var node_colors: Map[Node, Color] = null

    // Stack of nodes to check
    var nodes_todo: Seq[Node] = null

    // Set of tree nodes created by the algorithm.
    var node_parents: Map[Node, Node] = null

    // The actual depth-first-search algorithm that we want to visualize
    def step(): Unit = {
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
    def get_node_graph_pos(node: Node): Vec2 = {
      return Vec2(
        (node.id % nodes_columns) * nodes_margin + graph_x,
        (node.id / nodes_columns) * nodes_margin + graph_y)
    }
*/
    def get_node_graph_pos(node: Node): Vec2 = {
      val index = nodes.indexOf(node)
      val angle = toRadians(360.0 / nodes.length * index)
      return Vec2(
        graph_x + (cos(angle) * nodes_layout_radius),
        graph_y + (sin(angle) * nodes_layout_radius))
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

    def get_node_tree_pos(node: Node): Vec2 = {
      val depth = get_node_tree_depth(node)
      val siblings = nodes.filter(get_node_tree_depth(_) == depth).sortBy(_.id)
      val index = siblings.indexOf(node)

      return Vec2(
        index * nodes_margin + tree_x,
        depth * nodes_margin + tree_y)
    }

    // Drawing stuff
    val real_window = new JfxDisplay(800, 600,"👉😎👉 Zoop!")
    val window = new DownScaleFilter(real_window, multisampling, multisampling)
    val font = new FontRenderer(
      "font.png",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz0123456789.,!?'\"-+=/\\%()<>:;_[]{}^µ", 8, 8)

    // Map of the displayed node colors, used for fading
    var displayed_node_colors: Map[Node, Color] = nodes.map(n => n -> Color.White).toMap
    var graph_node_circles: Map[Node, Circle] = nodes.map(n => n -> Circle(get_node_graph_pos(n), nodes_size / 2)).toMap


    val buttons = Array(
      Button(
        Rectangle(
          x1 = (window.width  * 0.7),
          y1 = (window.height * 0.9),
          x2 = (window.width  * 0.8),
          y2 = (window.height * 0.95)),
        text = "Next step",
        enabled = { () => null != nodes_todo && nodes_todo.nonEmpty },
        action  = { () => step() }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.85),
          y1 = (window.height * 0.9),
          x2 = (window.width  * 0.95),
          y2 = (window.height * 0.95)),
        text = "Reset",
        enabled = { () => true },
        action  = { () => reset() }))

    reset()
    var mouse_pos: Vec2 = Vec2.Origin

    while (true) {
      val start_time = System.nanoTime()

      // Handle User Inputs
      for (event <- real_window.events) {
        event match {
          case KeyEvent(KeyEvent.Key_Space, true) => step()

          case KeyEvent(KeyEvent.Mouse_1, true) =>
            nodes
              .filter  { node => Circle.contains_point(graph_node_circles(node), mouse_pos) }
              .foreach { node => select_node(node) }
            buttons
              .filter  { button => button.enabled() && Rectangle.contains_point(button.bounds, mouse_pos) }
              .foreach { button => button.action() }

          case MouseEvent(x, y, _) => mouse_pos = Vec2(x, y) * multisampling

          case _ =>
        }
      }

      // Rendering
      window.fill(Color.White)

      def draw_arrow(from: Vec2, to: Vec2, color: Color): Unit = {
        val diff = to - from
        val normal = Vec2.normalize(diff)
        val p1 = from
        val p2 = to - normal * (nodes_size / 2)

        val p3 = p2 - normal * arrow_tip_length - Vec2.turn_cw (normal) * arrow_tip_length / 2
        val p4 = p2 - normal * arrow_tip_length - Vec2.turn_ccw(normal) * arrow_tip_length / 2

        window.draw_line(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt, line_width, color)
        window.draw_line(p2.x.toInt, p2.y.toInt, p3.x.toInt, p3.y.toInt, line_width, color)
        window.draw_line(p2.x.toInt, p2.y.toInt, p4.x.toInt, p4.y.toInt, line_width, color)
      }

      def draw_graph_node(node: Node, border_color: Color, fill_color: Color): Unit = {
        val circle = graph_node_circles(node)
        val x = circle.center.x.toInt
        val y = circle.center.y.toInt
        window.fill_circle(x, y, circle.radius.toInt - nodes_border_width, fill_color)
        window.draw_circle(x, y, circle.radius.toInt, nodes_border_width, border_color)
        val text = String.valueOf(node.id)
        val text_x = x / multisampling - (font.char_width * text.length) / 2
        val text_y = y / multisampling - font.char_height / 2
        font.draw(real_window, text_x, text_y, text, border_color)
      }

      def draw_tree_node(node: Node, border_color: Color, fill_color: Color): Unit = {
        val center = get_node_tree_pos(node)
        val x = center.x.toInt
        val y = center.y.toInt
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
        val text = String.valueOf(node.id)
        val text_x = x / multisampling - (font.char_width * text.length) / 2
        val text_y = y / multisampling - font.char_height / 2
        font.draw(real_window, text_x, text_y, text, border_color)
      }

      def draw_button(button: Button): Unit = {
        window.fill_rect(
          x1 = button.bounds.left.toInt,
          y1 = button.bounds.top.toInt,
          x2 = button.bounds.right.toInt,
          y2 = button.bounds.bottom.toInt,
          color = Color.White)

        window.draw_rect(
          x1 = button.bounds.left.toInt,
          y1 = button.bounds.top.toInt,
          x2 = button.bounds.right.toInt,
          y2 = button.bounds.bottom.toInt,
          line_width = 2 * multisampling,
          color = if (button.enabled()) Color.Black else Color.Gray)

        font.draw(
          real_window,
          x = (button.bounds.center.x / multisampling).toInt - (font.char_width * button.text.length).toInt / 2,
          y = (button.bounds.center.y / multisampling).toInt - (font.char_height).toInt / 2,
          string = button.text,
          color = if (button.enabled()) Color.Black else Color.Gray)
      }

      for ((source, targets) <- edges; target <- targets) {
        if (source == target) { // if the edge is a loop, we need a special case to draw it
          val p1 = graph_node_circles(source).center
          val n = (p1 - Vec2(graph_x, graph_y)) / nodes_layout_radius

          val p2 = p1 + n * (nodes_margin / 2) + Vec2.turn_cw(n)  * (nodes_margin / 8)
          val p3 = p1 + n * (nodes_margin / 2) + Vec2.turn_ccw(n) * (nodes_margin / 8)

          window.draw_line(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt, line_width, Color.Black)
          window.draw_line(p2.x.toInt, p2.y.toInt, p3.x.toInt, p3.y.toInt, line_width, Color.Black)
          draw_arrow(p3, p1, Color.Black)
        } else {
          val p1 = graph_node_circles(source).center
          val p2 = graph_node_circles(target).center
          val color = if (nodes_todo.contains(target) && node_parents.get(target).contains(source)) Color.Red else Color.Black
          draw_arrow(p1, p2, color)
        }
      }

      if (null != root) {
        val circle = graph_node_circles(root)
        window.fill_circle(circle.center.x.toInt, circle.center.y.toInt, circle.radius.toInt + 3 * multisampling, Color.Red)
      }

      for (node <- nodes) {
        val border_color = if (nodes_todo.contains(node)) Color.Red else Color.Black
        val fill_color = (displayed_node_colors(node) + node_colors(node)) * 0.5f
        displayed_node_colors += (node -> fill_color)
        draw_graph_node(node, border_color, fill_color)
      }

      for ((target, source) <- node_parents) {
        val p1 = get_node_tree_pos(source)
        val p2 = get_node_tree_pos(target)
        draw_arrow(p1, p2, Color.Black)
      }

      for (node <- nodes) {
        if (node == root || node_parents.contains(node)) {
          draw_tree_node(node, Color.Black, Color.White)
        }
      }

      buttons.foreach { draw_button(_) }

      // Wait for Frame time and draw to screen
      val end_time = System.nanoTime()
      val dur_ms = Math.ceil((end_time - start_time) / 1000000).toInt
      if (dur_ms < 30) Thread.sleep(33 - dur_ms)

      window.commit()
    }
  }
}