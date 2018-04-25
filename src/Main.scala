import java.io.IOException
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
    action   : () => Unit,
    enabled  : () => Boolean = { () => true },
    highlight: () => Boolean = { () => false })

  // Entry point of the Program
  def main(args: Array[String]): Unit = {

    //load defaults

    // Set of all default Nodes
   val initial_nodes =
      Seq(
        Node(id = 0),
        Node(id = 1),
        Node(id = 2),
        Node(id = 3),
        Node(id = 4),
        Node(id = 5),
        Node(id = 6),
        Node(id = 7),
        Node(id = 8))

    // default Adjacency List of the Nodes
    type Edges = Seq[(Node, Seq[Node])]
    val initial_edges =
      Seq[(Node, Seq[Node])](
        (initial_nodes(0), Seq(initial_nodes(1), initial_nodes(3))),
        (initial_nodes(1), Seq(initial_nodes(3), initial_nodes(4))),
        (initial_nodes(2), Seq(initial_nodes(5))),
        (initial_nodes(3), Seq(initial_nodes(6), initial_nodes(7))),
        (initial_nodes(4), Seq(initial_nodes(3), initial_nodes(7))),
        (initial_nodes(5), Seq(initial_nodes(5), initial_nodes(8))),
        (initial_nodes(6), Seq()),
        (initial_nodes(7), Seq(initial_nodes(5), initial_nodes(7))),
        (initial_nodes(8), Seq()))

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

    // stacks for undo and redo. The head of the history stack is the current state
    var history = Seq(initial_edges)
    var future = Seq[Edges]()

    // The actual depth-first-search algorithm that we want to visualize
    def step(): Unit = {
      // If there are still nodes on the stack
      if (nodes_todo.nonEmpty) {

        // take the top one
        val current_node = nodes_todo.head

        // set its color to gray
        nodes_color += (current_node -> Color.Gray)

        // go through the adjacency list to find the neighbors of the current node
        for ((source, targets) <- current_state()) {
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

    def load_graph(): Either[Edges, String] = {
      val bytes = Files.readAllBytes(Paths.get("main_graph.ini"))
      val input = new String(bytes, StandardCharsets.UTF_8)

      var nodes = Seq[Node]()
      var edges = Seq[(Node, Seq[Node])]()

      def get_node(id: Int): Node = {
        nodes.find(_.id == id).getOrElse {
          val node = Node(id)
          nodes = nodes :+ node
          node
        }
      }

      var i = 0

      def error() = Right(s"Error whole parsing at position $i")

      def skip_whitespaces(): Unit = while (i < input.length && input(i).isWhitespace) i += 1;
      def char(c: Char): Boolean = {
        skip_whitespaces()
        val same = input(i) == c
        if (same) i += 1
        return same
      }
      def string(s: String): Boolean = {
        skip_whitespaces()
        var j = 0
        while (j < s.length && j + i < input.length) {
          if (input(i + j) != s(j)) {
            return false
          }
          j += 1
        }
        i += j
        return true
      }
      def int(): Int = {
        skip_whitespaces()
        var digits = ""
        while (i < input.length && input(i).isDigit) {
          digits += input(i)
          i += 1
        }
        return if (digits.isEmpty) -1 else Integer.parseInt(digits)
      }

      if (!string("graph")) return error()
      if (!char('=')) return error()
      while (i < input.length) {
        val source_id = int()
        if (source_id < 0) return error()
        val source = get_node(source_id)
        nodes = source +: nodes
        var targets = Seq[Node]()

        if (!char('(')) return error()
        var target_id = int()
        while (0 <= target_id) {
          targets = targets :+ get_node(target_id)
          char(',')
          target_id = int()
        }
        if (!char(')')) return error()
        if (!char(';')) return error()
        skip_whitespaces()
        edges = edges :+ (source, targets)
      }

      return Left(edges)
    }

    def graph_to_string(): String = {
      val builder = new StringBuilder()
      builder.append("graph=")

      for ((source, targets) <- current_state()) {
         builder.append(source.id)
         builder.append("(")
         for (target <- targets) {
            builder.append(target.id)
            builder.append(",")
         }
         builder.append(");")
      }
      return builder.toString
    }

    def tree_to_string(): String = {
      val builder = new StringBuilder()
      builder.append("tree=")

      for ((child, parent) <- node_parents) {
        builder.append(parent.id)
        builder.append("->")
        builder.append(child.id)
        builder.append(";")
      }
      return builder.toString
    }

    def save_graph_and_tree(): Boolean = {
      try {
        val graph_string = graph_to_string()
        Files.write(Paths.get("main_graph.ini"), graph_string.getBytes(StandardCharsets.UTF_8))
      } catch {
        case _: IOException => return false
      }
      try {
        val tree_string = tree_to_string()
        Files.write(Paths.get("main_tree.ini"), tree_string.getBytes(StandardCharsets.UTF_8))
      } catch {
        case _: IOException => return false
      }

      return true
    }

    def undo(): Unit = {
      if (1 < history.length) {
        val head = history.head
        history = history.tail
        future = head +: future
      }
    }

    def redo(): Unit = {
      if (0 < future.length) {
        val head = future.head
        future = future.tail
        history = head +: history
      }
    }

    def current_state(): Edges = {
      return history.head
    }

    def push_state(state: Edges): Unit = {
      history = state +: history
      future = Seq[Seq[(Node, Seq[Node])]]()
    }

    def add_node(): Unit = {
      val new_node = Node(id = current_state().length + 1)
      push_state(current_state :+ (new_node, Seq[Node]()))
      reset()
    }

    def delete_node(): Unit = {
      val selected = root
      if (null != selected) {
        push_state(
          current_state().filter {
            case (`selected`, _) => false
            case _               => true
          }.map {
            case (source, targets) => (source, targets.filter(_ != selected))
          })
      }
      set_root(null)
    }

    def add_edge(from: Node, to: Node): Unit = {
      push_state(
        current_state().map {
          case (`from`, targets) if !targets.contains(to) => (from, to +: targets)
          case t => t
        })
      set_root(null)
    }

    def delete_edge(from: Node, to: Node): Unit = {
      push_state(
        current_state().map {
          case (`from`, targets) => (from, targets.filter(_ != to))
          case t => t
        })
      set_root(null)
    }

    // Useful functions
    def reset(): Unit = {
      nodes_color = current_state().map { case (n, _) => n -> Color.White }.toMap
      nodes_depth = current_state().map { case (n, _) => n -> -1 }.toMap
      if (null != root) {
        nodes_depth += (root -> 0)
        nodes_todo = Seq(root)
      } else {
        nodes_todo = Seq()
      }
      node_parents = Map[Node, Node]()
    }

    def set_root(node: Node): Unit = {
      root = node
      reset()
    }

    // Layouting for the graph and tree
    def get_node_graph_pos(node: Node): Vec2 = {
      val index = current_state().indexWhere {
        case (`node`, _) => true
        case _ => false
      }
      val angle = toRadians(360.0 / current_state().length * index)
      return Vec2(
        graph_x + (cos(angle) * nodes_layout_radius),
        graph_y + (sin(angle) * nodes_layout_radius))
    }

    def get_node_tree_pos(node: Node): Vec2 = {
      val depth    = nodes_depth(node)
      val siblings = current_state().map { case (n, _) => n }.filter(nodes_depth(_) == depth).sortBy(_.id)
      val index    = siblings.indexOf(node)

      return Vec2(
        index * nodes_margin + tree_x,
        depth * nodes_margin + tree_y)
    }

    // Drawing stuff
    val window = new JfxDisplay(800, 600,"👉😎👉 Zoop!")
    val font_renderer = new FontRenderer(FontRenderer.load("Arial.fnt"))

    // Map of the displayed node colors, used for fading
    var displayed_nodes_color = Map[Node, Color]()

    val Tool_None        = 0
    val Tool_Add_Edge    = 1
    val Tool_Delete_Edge = 2
    var active_tool = Tool_None

    val buttons = Array(
      Button(
        Rectangle(
          x1 = (window.width  * 0.0),
          y1 = (window.height * 0.95),
          x2 = (window.width  * 0.15),
          y2 = (window.height * 1.0)),
        text   = "Add Node",
        action = { () => add_node() }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.15),
          y1 = (window.height * 0.95),
          x2 = (window.width  * 0.3),
          y2 = (window.height * 1.0)),
        text      = "Add Edge",
        action    = { () =>
          if (active_tool != Tool_Add_Edge)
            active_tool = Tool_Add_Edge
          else
            active_tool = Tool_None
        },
        highlight = { () => active_tool == Tool_Add_Edge }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.0),
          y1 = (window.height * 0.9),
          x2 = (window.width  * 0.15),
          y2 = (window.height * 0.95)),
        text    = "Delete Node",
        action  = { () => delete_node() },
        enabled = { () => null != root }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.15),
          y1 = (window.height * 0.9),
          x2 = (window.width  * 0.3),
          y2 = (window.height * 0.95)),
        text      = "Delete Edge",
        action    = { () =>
          if (active_tool != Tool_Delete_Edge)
            active_tool = Tool_Delete_Edge
          else
            active_tool = Tool_None
        },
        highlight = { () => active_tool == Tool_Delete_Edge }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.35),
          y1 = (window.height * 0.95),
          x2 = (window.width  * 0.5),
          y2 = (window.height * 1.0)),
        text   = "Load",
        action = { () =>
          load_graph() match {
            case Left(loaded_edges) =>
              push_state(loaded_edges)
              reset()
            case Right(error) =>
              println(error)
          }
        }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.5),
          y1 = (window.height * 0.95),
          x2 = (window.width  * 0.65),
          y2 = (window.height * 1.0)),
        text   = "Save",
        action = { () => save_graph_and_tree() }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.35),
          y1 = (window.height * 0.9),
          x2 = (window.width  * 0.5),
          y2 = (window.height * 0.95)),
        text   = "Undo",
        action = { () => undo() },
        enabled = { () => 1 < history.length }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.5),
          y1 = (window.height * 0.9),
          x2 = (window.width  * 0.65),
          y2 = (window.height * 0.95)),
        text    = "Redo",
        action  = { () => redo() },
        enabled = { () => 0 < future.length }),


      Button(
        Rectangle(
          x1 = (window.width  * 0.7),
          y1 = (window.height * 0.95),
          x2 = (window.width  * 0.85),
          y2 = (window.height * 1.0)),
        text    = "Next step",
        action  = { () => step() },
        enabled = { () => null != root && null != nodes_todo && nodes_todo.nonEmpty }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.85),
          y1 = (window.height * 0.95),
          x2 = (window.width  * 1.0),
          y2 = (window.height * 1.0)),
        text   = "Reset",
        action = { () => reset() }))

    reset()
    var mouse_pos: Vec2 = Vec2.Origin

    while (true) {
      val start_time = System.nanoTime()

      def graph_node_circle(node: Node) = Circle(get_node_graph_pos(node), nodes_size / 2)

      // Handle User Inputs
      for (event <- window.events) {
        event match {
          case KeyEvent(KeyEvent.Key_Space, true) => step()

          case KeyEvent(KeyEvent.Mouse_1, true) =>
            current_state()
              .map     { case (node, _) => node }
              .filter  { node => Circle.contains_point(graph_node_circle(node), mouse_pos) }
              .foreach { node =>
                if (null != root && active_tool == Tool_Add_Edge)
                  add_edge(root, node)
                else if (null != root && active_tool == Tool_Delete_Edge)
                  delete_edge(root, node)
                else
                  set_root(node)
              }
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

      def draw_graph_node(node: Node, border_color: Color, fill_color: Color, text_color: Color): Unit = {
        val circle = graph_node_circle(node)
        val x = circle.center.x.toInt
        val y = circle.center.y.toInt
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
        val text = String.valueOf(node.id)
        val (label_width, label_height) = font_renderer.calculate_bounds(text, font_size)
        val text_x = x - label_width / 2
        val text_y = y + 6
        font_renderer.draw(window, text_x, text_y, text, font_size, 0.5f, text_color)
      }

      def draw_tree_node(node: Node, border_color: Color, fill_color: Color, text_color: Color): Unit = {
        val center = get_node_tree_pos(node)
        val x = center.x.toInt
        val y = center.y.toInt
        window.fill_circle(x, y, nodes_size / 2, border_color)
        window.fill_circle(x, y, nodes_size / 2 - nodes_border_width, fill_color)
        val text = String.valueOf(node.id)
        val (label_width, label_height) = font_renderer.calculate_bounds(text, font_size)
        val text_x = x - label_width / 2
        val text_y = y + 6
        font_renderer.draw(window, text_x, text_y, text, font_size, 0.5f, text_color)
      }

      def draw_button(button: Button): Unit = {
        val fill_color = if (button.highlight() || Rectangle.contains_point(button.bounds, mouse_pos)) Color.Light_Gray else Color.White
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

      for ((source, targets) <- current_state(); target <- targets) {
        if (source == target) { // if the edge is a loop, we need a special case to draw it
          val p1 = graph_node_circle(source).center
          val n = (p1 - Vec2(graph_x, graph_y)) / nodes_layout_radius

          val p2 = p1 + n * (nodes_margin / 2) + Vec2.turn_cw(n)  * (nodes_margin / 8)
          val p3 = p1 + n * (nodes_margin / 2) + Vec2.turn_ccw(n) * (nodes_margin / 8)

          window.draw_line(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt, line_width, Color.Black)
          window.draw_line(p2.x.toInt, p2.y.toInt, p3.x.toInt, p3.y.toInt, line_width, Color.Black)
          draw_arrow(p3, p1, Color.Black)
        } else {
          val p1 = graph_node_circle(source).center
          val p2 = graph_node_circle(target).center
          val color = if (nodes_todo.contains(target) && node_parents.get(target).contains(source)) Color.Red else Color.Black
          draw_arrow(p1, p2, color)
        }
      }

      if (null != root) {
        val circle = graph_node_circle(root)
        window.fill_circle(
          circle.center.x.toInt,
          circle.center.y.toInt,
          circle.radius.toInt + selection_border_width,
          Color.Red)
      }

      for ((node, _) <- current_state()) {
        val border_color = if (nodes_todo.contains(node)) Color.Red else Color.Black
        val old_color = displayed_nodes_color.getOrElse(node, Color.White)
        val fill_color = (old_color + nodes_color(node)) * 0.5f
        displayed_nodes_color += (node -> fill_color)
        val text_color = if (fill_color.avg < 0.5) Color.White else Color.Black
        draw_graph_node(node, border_color, fill_color, text_color)
      }

      for ((target, source) <- node_parents) {
        val p1 = get_node_tree_pos(source)
        val p2 = get_node_tree_pos(target)
        draw_arrow(p1, p2, Color.Black)
      }

      for ((node, _) <- current_state()) {
        if (node == root || node_parents.contains(node)) {
          draw_tree_node(node, Color.Black, Color.White, Color.Black)
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
