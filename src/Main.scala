import java.lang.Math._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import ui.Color
import ui.FontRenderer
import ui.JfxDisplay
import ui.KeyEvent
import ui.MouseEvent


object Main {

  // Drawing settings
  val multisampling          =   4
  val nodes_columns          =   3
  val nodes_margin           =  70
  val nodes_size             =  50
  val font_size              =  22
  val nodes_border_width     =   2
  val selection_border_width =   3
  val line_width             =   2
  val arrow_tip_length       =  10
  val nodes_layout_radius    = 150
  val graph_x                = 200
  val graph_y                = 200
  val tree_x                 = 450
  val tree_y                 =  50
  val button_border_width    =   2

  //edges and nodes

  // Datatype to represent the graph and tree
  case class Node(id: Int)

  // Datatypes for the UI
  case class Button(
    bounds: Rectangle,
    text: String,
    enabled: () => Boolean,
    action : () => Unit)

  // Entry point of the Program
  def main(args: Array[String]): Unit = {

    //load defaults

    // Set of all Nodes
   var nodes =
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
   var edges =
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
    var nodes_color: Map[Node, Color] = null

    // Depth of the Nodes inside the tree, for visualisation
    var nodes_depth: Map[Node, Int] = null

    // Stack of Nodes to check
    var nodes_todo: Seq[Node] = null

    // Map of the parent Nodes of each node. Null if Node has no parent
    var node_parents: Map[Node, Node] = null

    // The actual depth-first-search algorithm that we want to visualize
    def step(): Unit = {
      // If there are still nodes on the stack
      if (nodes_todo.nonEmpty) {

        // take the top one
        val current_node = nodes_todo.head

        // set its color to gray
        nodes_color += (current_node -> Color.Gray)

        // go through the adjacency list to find the neighbors of the current node
        for ((source, targets) <- edges) {
          if (source == current_node) {

            // check if any neighbors are still colored white
            targets.find(nodes_color(_) == Color.White) match {
              case Some(target) =>
                // If a white neighbor is found, it gets pushed on the stack and
                // its parent is set to the current node.
                node_parents += (target -> source)
                nodes_depth += (target -> (nodes_depth(source) + 1))
                nodes_todo = target +: nodes_todo
              case None =>
                // If no white neighbor is found, this node is finished. It will be
                // colored black and removed from the stack.
                nodes_color += (current_node -> Color.Black)
                nodes_todo = nodes_todo.tail
            }
          }
        }
      }
    }

    def load_graphs(): Boolean = {      
      return true;
    }

    def save_tree(): Boolean = {

      var str_edges = "edges="

      for ((source, targets) <- edges) {
         str_edges +=  source.id + "("
         for (target <- targets) {
            str_edges += target.id + ","
         }
         str_edges += ");"
      }

      Files.write(Paths.get("main_graph.ini"), str_edges.getBytes(StandardCharsets.UTF_8))
      return true;
    }

    // Useful functions
    def reset(): Unit = {
      if (null == root) root = nodes(0)
      nodes_color = nodes.map(n => n -> Color.White).toMap
      nodes_depth = nodes.map(n => n -> -1).toMap
      nodes_depth += (root -> 0)
      nodes_todo = Seq(root)
      node_parents = Map[Node, Node]()
    }

    def set_root(node: Node): Unit = {
      root = node
      reset()
    }

    // Layouting for the graph and tree
    def get_node_graph_pos(node: Node): Vec2 = {
      val index = nodes.indexOf(node)
      val angle = toRadians(360.0 / nodes.length * index)
      return Vec2(
        graph_x + (cos(angle) * nodes_layout_radius),
        graph_y + (sin(angle) * nodes_layout_radius))
    }

    def get_node_tree_pos(node: Node): Vec2 = {
      val depth    = nodes_depth(node)
      val siblings = nodes.filter(nodes_depth(_) == depth).sortBy(_.id)
      val index    = siblings.indexOf(node)

      return Vec2(
        index * nodes_margin + tree_x,
        depth * nodes_margin + tree_y)
    }

    // Drawing stuff
    val window = new JfxDisplay(800, 600,"👉😎👉 Zoop!")
    val font_renderer = new FontRenderer(FontRenderer.load("Arial.fnt"))

    // Map of the displayed node colors, used for fading
    var displayed_nodes_color: Map[Node, Color] = nodes.map(n => n -> Color.White).toMap
    var graph_node_circles: Map[Node, Circle] = nodes.map(n => n -> Circle(get_node_graph_pos(n), nodes_size / 2)).toMap


    val buttons = Array(
      Button(
        Rectangle(
          x1 = (window.width  * 0.3),
          y1 = (window.height * 0.875),
          x2 = (window.width  * 0.45),
          y2 = (window.height * 0.975)),
        text = "Load",
        enabled = { () => true },
        action  = { () => load_graphs() }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.475),
          y1 = (window.height * 0.875),
          x2 = (window.width  * 0.625),
          y2 = (window.height * 0.975)),
        text = "Save",
        enabled = { () => true },
        action  = { () => save_tree() }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.65),
          y1 = (window.height * 0.875),
          x2 = (window.width  * 0.8),
          y2 = (window.height * 0.975)),
        text = "Next step",
        enabled = { () => null != nodes_todo && nodes_todo.nonEmpty },
        action  = { () => step() }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.825),
          y1 = (window.height * 0.875),
          x2 = (window.width  * 0.975),
          y2 = (window.height * 0.975)),
        text = "Reset",
        enabled = { () => true },
        action  = { () => reset() }))

    reset()
    var mouse_pos: Vec2 = Vec2.Origin

    while (true) {
      val start_time = System.nanoTime()

      // Handle User Inputs
      for (event <- window.events) {
        event match {
          case KeyEvent(KeyEvent.Key_Space, true) => step()

          case KeyEvent(KeyEvent.Mouse_1, true) =>
            nodes
              .filter  { node => Circle.contains_point(graph_node_circles(node), mouse_pos) }
              .foreach { node => set_root(node) }
            buttons
              .filter  { button => button.enabled() && Rectangle.contains_point(button.bounds, mouse_pos) }
              .foreach { button => button.action() }

          case MouseEvent(x, y, _) => mouse_pos = Vec2(x, y)

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
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
        val text = String.valueOf(node.id)
        val (label_width, label_height) = font_renderer.calculate_bounds(text, font_size)
        val text_x = x - label_width / 2
        val text_y = y + 6
        font_renderer.draw(window, text_x, text_y, text, font_size, 0.5f, border_color)
      }

      def draw_tree_node(node: Node, border_color: Color, fill_color: Color): Unit = {
        val center = get_node_tree_pos(node)
        val x = center.x.toInt
        val y = center.y.toInt
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
        val text = String.valueOf(node.id)
        val (label_width, label_height) = font_renderer.calculate_bounds(text, font_size)
        val text_x = x - label_width / 2
        val text_y = y + 6
        font_renderer.draw(window, text_x, text_y, text, font_size, 0.5f, border_color)
      }

      def draw_button(button: Button): Unit = {
        val fill_color = if (Rectangle.contains_point(button.bounds, mouse_pos)) Color.Light_Gray else Color.White
        val border_color = if (button.enabled()) Color.Black else Color.Gray

        window.fill_rect(
          x1 = button.bounds.left.toInt,
          y1 = button.bounds.top.toInt,
          x2 = button.bounds.right.toInt,
          y2 = button.bounds.bottom.toInt,
          color = fill_color)

        window.draw_rect(
          x1 = button.bounds.left.toInt,
          y1 = button.bounds.top.toInt,
          x2 = button.bounds.right.toInt,
          y2 = button.bounds.bottom.toInt,
          line_width = button_border_width,
          color = border_color)

        val (label_width, label_height) = font_renderer.calculate_bounds(button.text, font_size)
        val text_x = (button.bounds.center.x).toInt - label_width / 2
        val text_y = (button.bounds.center.y).toInt + 6

        font_renderer.draw(window, text_x, text_y, button.text, font_size, 0.5f, border_color)
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
        window.fill_circle(
          circle.center.x.toInt,
          circle.center.y.toInt,
          circle.radius.toInt + selection_border_width,
          Color.Red)
      }

      for (node <- nodes) {
        val border_color = if (nodes_todo.contains(node)) Color.Red else Color.Black
        val fill_color = (displayed_nodes_color(node) + nodes_color(node)) * 0.5f
        displayed_nodes_color += (node -> fill_color)
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
      println(dur_ms)
      if (dur_ms < 30) Thread.sleep(33 - dur_ms)

      window.commit()
    }
  }
}
