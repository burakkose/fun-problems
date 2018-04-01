package net.koseburak.fun

object GraphCycle {

  sealed trait Color
  case object White extends Color
  case object Gray extends Color
  case object Black extends Color

  case class Node(d: Int)
  case class Vertex(out: List[Node] = List.empty[Node])
  import scala.collection.mutable
  val graph = Map(
    Node(0) -> Vertex(out = List(Node(1), Node(2))),
    Node(1) -> Vertex(out = List(Node(3))),
    Node(2) -> Vertex(out = List(Node(4), Node(5))),
    Node(3) -> Vertex(out = List(Node(6))),
    Node(6) -> Vertex(),
    Node(4) -> Vertex(),
    Node(5) -> Vertex()
  )

  def isCyclic(graph: Map[Node, Vertex]): Boolean = {

    val colors = mutable.Map[Node, Color]() ++ graph.mapValues(_ => White)

    import scala.annotation.tailrec

    def inner1(stack: List[Node]): Boolean = {
      stack match {
        case head :: tail =>
          colors(head) match {
            case Gray  => true
            case White =>
              colors += (head -> Gray)
              val result = inner1(graph(head).out ::: tail)
              colors += (head -> Black)
              result
            case Black => inner1(graph(head).out ::: tail)
          }
        case _ => false
      }
    }

    @tailrec
    def inner2(keys: List[Node]): Boolean = {
      keys match {
        case h :: t =>
          if (colors(h) == White) {
            val result = inner1(List(h))
            if(result)
              result
            else inner2(t)
          }
          else inner2(t)
        case _ => false
      }
    }
    inner2(graph.keys.toList)
  }

  isCyclic(graph)
}
