import java.io.IOException
import java.lang.Math._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import ui.Color
import ui.FontRenderer
import ui.JfxDisplay
import ui.KeyEvent
import ui.MouseEvent
import ui.SdfRenderer


object Main {

  // Drawing settings
  val nodes_margin           =  20
  val nodes_size             =  50
  val nodes_border_width     =   2
  val node_font_size         =  22
  val selection_border_width =   3
  val line_width             =   1
  val arrow_tip_length       =  10
  val nodes_layout_radius    = 150
  val graph_x                = 250
  val graph_y                = 310
  val tree_x                 = 500
  val tree_y                 = 160
  val button_border_width    =   2
  val button_font_size       =  20
  val table_width            =  50
  val table_x                = 170
  val table_y                =  10
  val iter_text_x            =  10
  val iter_text_y            =  70
  val run_speed_ms           = 500

  // Datatype to represent a Node in the graph and tree
  case class Node(id: Int)

  // Datatype that represents an Adjacency List
  type Edges = Seq[(Node, Seq[Node])]

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

    // Stack of Nodes to check during the DFS algorithm
    var nodes_todo: Seq[Node] = null

    // Map from Node to parent Node. Null if Node has no parent and/or is the root node
    var node_parents: Map[Node, Node] = null

    // Number of times, the step function was called and the todo stack wasn't empty
    var iterations = 0

    // Stacks for undo and redo. The head of the history stack is the current state
    var history = Seq(initial_edges)
    var future = Seq[Edges]()

    // Representations of the Graph and Tree Nodes
    var graph_circles = Map[Node, Circle]()
    var tree_circles = Map[Node, Circle]()

    // The actual depth-first-search algorithm that we want to visualize
    def step(): Unit = {
      // If there are still nodes on the stack
      if (nodes_todo.nonEmpty) {
        iterations += 1

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

    // Function that calculates the center of the graph, depending on the number of Nodes the graph currently contains.
    def get_graph_pos(): (Vec2, Double) = {
      val variable_radius = (current_state().length * (nodes_size + nodes_margin)) / (2 * PI)
      val extra_size = max(0, variable_radius - nodes_layout_radius)
      return (Vec2(graph_x - extra_size, graph_y + extra_size), max(variable_radius, nodes_layout_radius))
    }

    // Function that updates the layout of all graph Nodes. It is called if Nodes are added or deleted.
    def update_graph_circles(): Unit = {
      def get_node_direction(node: Node): Vec2 = {
        val index = current_state().indexWhere {
          case (`node`, _) => true
          case _ => false
        }
        val angle = toRadians(360.0 / current_state().length * index)
        return Vec2(cos(angle), sin(angle))
      }
      val (graph_pos, radius) = get_graph_pos()
      graph_circles = current_state().map {
        case (node, _) => (node, Circle(graph_pos + get_node_direction(node) * radius, nodes_size / 2))
      }.toMap
    }

    // Function that calculates the positions for all Circles in the Tree
    def update_tree_circles(): Unit = {
      def get_node_tree_pos(node: Node): Vec2 = {
        val depth    = nodes_depth(node)
        val siblings = current_state().map { case (n, _) => n }.filter(nodes_depth(_) == depth).sortBy(_.id)
        val index    = siblings.indexOf(node)

        return Vec2(
          index * (nodes_size + nodes_margin) + tree_x,
          depth * (nodes_size + nodes_margin) + tree_y)
      }
      tree_circles = current_state().filter {
        case (node, _) => root == node || node_parents.contains(node)
      }.map {
        case (node, _) => (node, Circle(get_node_tree_pos(node), nodes_size / 2))
      }.toMap
    }

    // Function that loads the Graph as an Adjacency List from a default file
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

      def error() = Right(s"Error while parsing at position $i")

      def skip_whitespaces(): Unit = while (i < input.length && input(i).isWhitespace) i += 1;
      def char(c: Char): Boolean = {
        skip_whitespaces()
        if (i < input.length && input(i) == c) {
          i += 1
          return true
        }
        return false
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
        if (j == s.length) {
          i += j
          return true
        }
        return false
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

    // Function that turns an Adjacency List into a String for serialization
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

    // Function that turns Parent Map into a String for serialization
    def tree_to_string(): String = {
      val builder = new StringBuilder()
      builder.append("graph=")

      // Turn the Map[C, P] into a Map[P, Set[C]]
      val node_children = node_parents.groupBy {case (_, v) => v }.mapValues(_.keys)

      for ((parent, children) <- node_children) {
        builder.append(parent.id)
        builder.append("(")
        for (child <- children) {
          builder.append(child.id)
          builder.append(",")
        }
        builder.append(");")
      }
      return builder.toString
    }

    // Function that saves both the Graph and Tree into default files
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

    // Function that reverts the last change
    def undo(): Unit = {
      if (1 < history.length) {
        val head = history.head
        history = history.tail
        future = head +: future
        root = null
        reset()
      }
    }

    // Function that reverts the last reversion
    def redo(): Unit = {
      if (0 < future.length) {
        val head = future.head
        future = future.tail
        history = head +: history
        root = null
        reset()
      }
    }

    // Returns the most current state
    def current_state(): Edges = {
      return history.head
    }

    // Saves the given state as the most current one
    def push_state(state: Edges): Unit = {
      history = state +: history
      future = Seq[Seq[(Node, Seq[Node])]]()
      reset()
    }

    // Appends a new Node onto the Adjacency List
    def add_node(): Unit = {
      val new_node = Node(id = current_state().length)
      push_state(current_state :+ (new_node, Seq[Node]()))
    }

    // Removes the given node from the Adjacency List
    // Also makes sure that all edges towards the node are removed
    def delete_node(selected: Node): Unit = {
      if (null != selected) {
        push_state(
          current_state().filter {
            case (`selected`, _) => false
            case _               => true
          }.map {
            case (source, targets) => (source, targets.filter(_ != selected))
          })
      }
    }

    // Creates a new Edge between the two given Nodes. Even if both nodes are the same.
    def add_edge(from: Node, to: Node): Unit = {
      push_state(
        current_state().map {
          case (`from`, targets) if !targets.contains(to) => (from, to +: targets)
          case t => t
        })
    }

    // Deletes the Edge between the two given Nodes. Even if both nodes are the same.
    def delete_edge(from: Node, to: Node): Unit = {
      push_state(
        current_state().map {
          case (`from`, targets) => (from, targets.filter(_ != to))
          case t => t
        })
    }

    // Deletes the Tree and resets all internal maps like Color, Depth, etc
    def reset(): Unit = {
      nodes_color = current_state().map { case (n, _) => n -> Color.White }.toMap
      nodes_depth = current_state().map { case (n, _) => n -> -1 }.toMap
      if (null != root) {
        nodes_depth += (root -> 0)
        nodes_todo = Seq(root)
      } else {
        nodes_todo = Seq()
      }
      iterations = 0
      node_parents = Map[Node, Node]()
      update_graph_circles()
      update_tree_circles()
    }

    // Sets the root to the given node and resets if the algorithm wasn't running
    def set_root(node: Node): Unit = {
      if (0 == iterations) {
        root = node
        reset()
      }
    }

    // Drawing stuff
    val window = new JfxDisplay(800, 600,"Depth First Search")
    val font_renderer = new FontRenderer(FontRenderer.load("Arial.fnt"))

    // Selectable modes
    val Mode_None        = 0
    val Mode_Add_Edge    = 1
    val Mode_Delete_Edge = 2
    val Mode_Run         = 3
    var active_mode = Mode_None

    // Time used for the Run mode
    var last_step_time = 0L

    // List of all buttons in this program
    val buttons = Array(
      Button(
        Rectangle(
          x1 = (window.width  * 0.0),  y1 = (window.height * 0.95),
          x2 = (window.width  * 0.15), y2 = (window.height * 1.0)),
        text    = "Add Node",
        enabled = { () => 0 == iterations },
        action  = { () => add_node() }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.15), y1 = (window.height * 0.95),
          x2 = (window.width  * 0.3),  y2 = (window.height * 1.0)),
        text      = "Add Edge",
        action    = { () =>
          if (active_mode != Mode_Add_Edge) {
            active_mode = Mode_Add_Edge
            set_root(null)
          } else {
            active_mode = Mode_None
          }
        },
        enabled   = { () => 0 == iterations },
        highlight = { () => active_mode == Mode_Add_Edge }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.0),  y1 = (window.height * 0.9),
          x2 = (window.width  * 0.15), y2 = (window.height * 0.95)),
        text    = "Delete Node",
        action  = { () => delete_node(root); set_root(null) },
        enabled = { () => null != root && 0 == iterations }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.15), y1 = (window.height * 0.9),
          x2 = (window.width  * 0.3),  y2 = (window.height * 0.95)),
        text      = "Delete Edge",
        action    = { () =>
          if (active_mode != Mode_Delete_Edge) {
            active_mode = Mode_Delete_Edge
            set_root(null)
          } else {
            active_mode = Mode_None
          }
        },
        enabled   = { () => 0 == iterations },
        highlight = { () => active_mode == Mode_Delete_Edge }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.35), y1 = (window.height * 0.95),
          x2 = (window.width  * 0.5),  y2 = (window.height * 1.0)),
        text   = "Load",
        action = { () =>
          load_graph() match {
            case Left(loaded_edges) =>
              push_state(loaded_edges)
            case Right(error) =>
              println(error)
          }
        }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.5),  y1 = (window.height * 0.95),
          x2 = (window.width  * 0.65), y2 = (window.height * 1.0)),
        text   = "Save",
        action = { () => save_graph_and_tree() }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.35), y1 = (window.height * 0.9),
          x2 = (window.width  * 0.5),  y2 = (window.height * 0.95)),
        text   = "Undo",
        action = { () => undo() },
        enabled = { () => 1 < history.length && 0 == iterations }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.5),  y1 = (window.height * 0.9),
          x2 = (window.width  * 0.65), y2 = (window.height * 0.95)),
        text    = "Redo",
        action  = { () => redo() },
        enabled = { () => 0 < future.length && 0 == iterations }),

      Button(
        Rectangle(
          x1 = (window.width  * 0.7),  y1 = (window.height * 0.95),
          x2 = (window.width  * 0.85), y2 = (window.height * 1.0)),
        text    = "Next step",
        action  = { () => step(); update_tree_circles() },
        enabled = { () => null != root && null != nodes_todo && nodes_todo.nonEmpty }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.7),  y1 = (window.height * 0.9),
          x2 = (window.width  * 0.85), y2 = (window.height * 0.95)),
        text    = "Run",
        action  = { () =>
          if (active_mode != Mode_Run) {
            active_mode = Mode_Run
            last_step_time = System.currentTimeMillis()
          } else {
            active_mode = Mode_None
          }
        },
        enabled   = { () => null != root && null != nodes_todo && nodes_todo.nonEmpty },
        highlight = { () => active_mode == Mode_Run }),
      Button(
        Rectangle(
          x1 = (window.width  * 0.85), y1 = (window.height * 0.95),
          x2 = (window.width  * 1.0),  y2 = (window.height * 1.0)),
        text   = "Reset",
        action = { () => reset() }))

    var mouse_pos: Vec2 = Vec2.Origin
    var offset: Vec2 = Vec2.Origin

    var redraw_graphs  = true
    var redraw_buttons = true

    var hover = Set[Button]()

    // Reset everything to get to a valid starting state
    reset()
    update_graph_circles()
    update_tree_circles()

    // Main loop. It waits for events, handles then and then draws everything to the screen.
    while (true) {
      if (!((active_mode == Mode_Run) || redraw_graphs || redraw_buttons)) {
        window.events.await_event()
      }

      val start_time = System.nanoTime()

      // Handle User Inputs
      for (event <- window.events) {
        event match {
          case KeyEvent(KeyEvent.Key_Space, true) =>
            step()
            update_tree_circles()
            redraw_graphs  = true
            redraw_buttons = true

          case KeyEvent(KeyEvent.Key_Escape, true) =>
            set_root(null)
            redraw_graphs  = true
            redraw_buttons = true

          case KeyEvent(KeyEvent.Mouse_1, true) =>
            current_state()
              .map     { case (node, _) => node }
              .filter  { node => Circle.contains_point(graph_circles(node), mouse_pos + offset) }
              .foreach { node =>
                if (null != root && active_mode == Mode_Add_Edge) {
                  add_edge(root, node)
                  set_root(null)
                } else if (null != root && active_mode == Mode_Delete_Edge) {
                  delete_edge(root, node)
                  set_root(null)
                } else {
                  set_root(node)
                }
                redraw_graphs  = true
                redraw_buttons = true
              }
            buttons
              .filter  { button => button.enabled() && Rectangle.contains_point(button.bounds, mouse_pos) }
              .foreach { button =>
                button.action()
                redraw_graphs  = true
                redraw_buttons = true
              }

          case KeyEvent(KeyEvent.Mouse_1, false) =>
            redraw_buttons = true

          case MouseEvent(x, y, dragged) =>
            val new_pos = Vec2(x, y)
            if (dragged) {
              offset += mouse_pos - new_pos
              redraw_graphs = true
            }
            mouse_pos = new_pos
            val new_hover = buttons.filter { button => Rectangle.contains_point(button.bounds, mouse_pos) }.toSet
            if (!dragged && hover != new_hover) redraw_buttons = true
            hover = new_hover

          case _ =>
        }
      }

      if (active_mode == Mode_Run) {
        val time_now = System.currentTimeMillis()
        if (time_now - last_step_time > run_speed_ms) {
          last_step_time = time_now
          if (nodes_todo.isEmpty) {
            active_mode = Mode_None
          } else {
            step()
            update_tree_circles()
            redraw_graphs  = true
            redraw_buttons = true
          }
        }
      }

      if (redraw_graphs) {
        redraw_graphs = false

        // Rendering
        window.fill(Color.White)

        // Function that draws an arrow between the two given points
        def draw_arrow(from: Vec2, to: Vec2, color: Color): Unit = {
          val diff = to - from
          val normal = Vec2.normalize(diff)
          val p1 = from
          val p2 = to - normal * (nodes_size / 2)
          val p3 = p2 - normal * (arrow_tip_length / 2)

          val p4 = p2 - normal * arrow_tip_length - Vec2.turn_cw (normal) * arrow_tip_length / 2
          val p5 = p2 - normal * arrow_tip_length - Vec2.turn_ccw(normal) * arrow_tip_length / 2

          SdfRenderer.render(
            window,
            SdfRenderer.unite(
              SdfRenderer.line(
                p1.x.toFloat, p1.y.toFloat,
                p3.x.toFloat, p3.y.toFloat,
                line_width),
              SdfRenderer.triangle(
                p2.x.toFloat, p2.y.toFloat,
                p4.x.toFloat, p4.y.toFloat,
                p5.x.toFloat, p5.y.toFloat)),
            color)
        }

        // Draws a possibly colored circle with an outline and some text inside
        def draw_node(node: Node, position: Vec2, border_color: Color, fill_color: Color, text_color: Color): Unit = {
          val x = (position.x - offset.x).toInt
          val y = (position.y - offset.y).toInt
          SdfRenderer.render(window, SdfRenderer.circle(x, y, (nodes_size - nodes_border_width) / 2), fill_color)
          SdfRenderer.render(window, SdfRenderer.ring(x, y, nodes_size / 2 - nodes_border_width, nodes_size / 2), border_color)
          val text = String.valueOf(node.id)
          val (label_width, label_height) = font_renderer.calculate_bounds(text, node_font_size)
          val text_x = x - label_width / 2
          val text_y = y + 6
          font_renderer.draw(window, text_x, text_y, text, node_font_size, 0.5f, text_color)
        }


        // Iterate over all edges and draw arrows between the nodes
        for ((source, targets) <- current_state(); target <- targets) {
          if (source == target) { // if the edge is a loop, we need a special case to draw it
            val p1 = graph_circles(source).center - offset
            val (graph_pos, radius) = get_graph_pos()
            val n = (p1 - graph_pos + offset) / radius

            val p2 = p1 + n * (nodes_size / 2 + nodes_margin * 0.75) + Vec2.turn_cw(n)  * (nodes_margin * 0.25)
            val p3 = p1 + n * (nodes_size / 2 + nodes_margin * 0.75) + Vec2.turn_ccw(n) * (nodes_margin * 0.25)

            SdfRenderer.render(window, SdfRenderer.line(p1.x.toFloat, p1.y.toFloat, p2.x.toFloat, p2.y.toFloat, line_width), Color.Black)
            SdfRenderer.render(window, SdfRenderer.line(p2.x.toFloat, p2.y.toFloat, p3.x.toFloat, p3.y.toFloat, line_width), Color.Black)
            draw_arrow(p3, p1, Color.Black)
          } else {
            val p1 = graph_circles(source).center - offset
            val p2 = graph_circles(target).center - offset
            val color = if (nodes_todo.contains(target) && node_parents.get(target).contains(source)) Color.Red else Color.Black
            draw_arrow(p1, p2, color)
          }
        }

        // Highlight the current root Node
        if (null != root) {
          val circle = graph_circles(root)
          SdfRenderer.render(
            window,
            SdfRenderer.circle(
              (circle.center.x - offset.x).toInt,
              (circle.center.y - offset.y).toInt,
              (circle.radius + selection_border_width).toFloat),
            Color.Red)
        }

        // Iterate over all Circles in the Graph and draw them
        for ((node, circle) <- graph_circles) {
          val border_color = if (nodes_todo.contains(node)) Color.Red else Color.Black
          val fill_color = nodes_color(node)
          val text_color = if (fill_color.avg < 0.5) Color.White else Color.Black
          draw_node(node, circle.center, border_color, fill_color, text_color)
        }

        // Iterate over all edges in the Tree and draw arrows for them
        for ((target, source) <- node_parents) {
          val p1 = tree_circles(source).center - offset
          val p2 = tree_circles(target).center - offset
          draw_arrow(p1, p2, Color.Black)
        }

        // Iterate over all Circles in the Tree and draw them
        for ((node, circle) <- tree_circles) {
          draw_node(node, circle.center, Color.Black, Color.White, Color.Black)
        }

        // Display a String with the number of iterations
        val iterations_text = s"Iterations: $iterations"
        font_renderer.draw(
          window,
          iter_text_x - offset.x.toInt,
          iter_text_y - offset.y.toInt,
          iterations_text,
          node_font_size,
          0.5f,
          Color.Black)

        // Draw the two horizontal tines for the Table
        SdfRenderer.render(
          window,
          SdfRenderer.line(
            table_x                                              - offset.x.toFloat, table_y + 33 - offset.y.toFloat,
            table_x + (current_state().length + 1) * table_width - offset.x.toFloat, table_y + 33 - offset.y.toFloat,
            line_width),
          Color.Black)

        SdfRenderer.render(
          window,
          SdfRenderer.line(
            table_x                                              - offset.x.toFloat, table_y + 66 - offset.y.toFloat,
            table_x + (current_state().length + 1) * table_width - offset.x.toFloat, table_y + 66 - offset.y.toFloat,
            line_width),
          Color.Black)

        // Draw the first column for the Table
        val pi_text = "π[n]"
        val (pi_label_width, _) = font_renderer.calculate_bounds(pi_text, node_font_size)
        val pi_text_x = table_x + table_width / 2 - pi_label_width / 2 - offset.x.toInt
        val pi_text_y = table_y + 24 - offset.y.toInt + 33
        font_renderer.draw(window, pi_text_x, pi_text_y, pi_text, node_font_size, 0.5f, Color.Black)

        val de_text = "d[n]"
        val (de_label_width, _) = font_renderer.calculate_bounds(de_text, node_font_size)
        val de_text_x = table_x + table_width / 2 - de_label_width / 2 - offset.x.toInt
        val de_text_y = table_y + 24 - offset.y.toInt + 66
        font_renderer.draw(window, de_text_x, de_text_y, de_text, node_font_size, 0.5f, Color.Black)

        // Iterate over all Nodes and draw an entry in the Table
        for ((node, _) <- current_state()) {
          val index = 1 + current_state().indexWhere {
            case (`node`, _) => true
            case _ => false
          }

          SdfRenderer.render(
            window,
            SdfRenderer.line(
              table_x + index * table_width - offset.x.toFloat, table_y       - offset.y.toFloat,
              table_x + index * table_width - offset.x.toFloat, table_y + 100 - offset.y.toFloat,
              line_width),
            Color.Black)

          val id_text = String.valueOf(node.id)
          val (id_label_width, _) = font_renderer.calculate_bounds(id_text, node_font_size)
          val id_text_x = table_x + index * table_width + table_width / 2 - id_label_width / 2 - offset.x.toInt
          val id_text_y = table_y + 24 - offset.y.toInt
          font_renderer.draw(window, id_text_x, id_text_y, id_text, node_font_size, 0.5f, Color.Black)

          val parent_text = node_parents.get(node).map { parent => String.valueOf(parent.id) }.getOrElse("-")
          val (parent_label_width, _) = font_renderer.calculate_bounds(parent_text, node_font_size)
          val parent_text_x = table_x + index * table_width + table_width / 2 - parent_label_width / 2 - offset.x.toInt
          val parent_text_y = table_y + 24 - offset.y.toInt + 33
          font_renderer.draw(window, parent_text_x, parent_text_y, parent_text, node_font_size, 0.5f, Color.Black)

          val depth_text = if (-1 == nodes_depth(node)) "∞" else String.valueOf(nodes_depth(node))
          val (depth_label_width, _) = font_renderer.calculate_bounds(depth_text, node_font_size)
          val depth_text_x = table_x + index * table_width + table_width / 2 - depth_label_width / 2 - offset.x.toInt
          val depth_text_y = table_y + 24 - offset.y.toInt + 66
          font_renderer.draw(window, depth_text_x, depth_text_y, depth_text, node_font_size, 0.5f, Color.Black)
        }
      }

      if (redraw_buttons) {
        redraw_buttons = false

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

          val (label_width, label_height) = font_renderer.calculate_bounds(button.text, button_font_size)
          val text_x = (button.bounds.center.x).toInt - label_width / 2
          val text_y = (button.bounds.center.y).toInt + 6

          font_renderer.draw(window, text_x, text_y, button.text, button_font_size, 0.5f, border_color)
        }

        buttons.foreach { draw_button(_) }
      }

      // Wait for Frame time and draw to screen
      val end_time = System.nanoTime()
      val dur_ms = Math.ceil((end_time - start_time) / 1000000).toInt
      //println(dur_ms)
      if (dur_ms < 30) Thread.sleep(33 - dur_ms)

      window.commit()
    }
  }
}
