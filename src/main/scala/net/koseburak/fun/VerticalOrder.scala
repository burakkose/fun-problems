package net.koseburak.fun

object VerticalOrder {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree
  import scala.annotation.tailrec
  def printVertical(tree: Tree): Map[Int, Seq[Tree]] = {
    @tailrec
    def inner(queue: List[(Tree, Int)],
              map: Map[Int, Seq[Tree]]): Map[Int, Seq[Tree]] = {
      if (queue.isEmpty) map
      else {
        val (node, hd) = queue.head
        node match {
          case Nil => inner(queue.tail, map)
          case Node(_, l, r) =>
            val order = map.getOrElse(hd, Seq.empty[Tree]) :+ node
            val newMap = map + (hd -> order)
            val newQueue = queue.tail ::: List((l, hd - 1), (r, hd + 1))
            inner(newQueue, newMap)
        }
      }
    }
    inner(List(tree -> 0), Map.empty[Int, Seq[Tree]])
  }

  val tree = Node(1,
                  Node(2, Node(4), Node(5)),
                  Node(3, Node(6, Nil, Node(8)), Node(7, Nil, Node(9))))

  printVertical(tree).toList
    .sortBy(_._1)
    .map(_._2.map(_.asInstanceOf[Node].value).mkString(" "))
    .mkString("\n")
}
