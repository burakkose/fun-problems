package net.koseburak.fun

object ConnectNodes {
  sealed trait Tree
  case class Node(value: Int,
                  left: Tree = Nil,
                  right: Tree = Nil,
                  var next: Option[Tree] = None)
      extends Tree
  case object Nil extends Tree

  def connect(tree: Tree): Tree = {
    def inner(queue: List[Tree]): Unit = {
      if (queue.isEmpty) ()
      else {
        val peek = queue.head
        peek match {
          case Nil => inner(queue.tail)
          case n: Node =>
            if (queue.tail.nonEmpty) {
              val tailHead = queue.tail.head
              tailHead match {
                case Nil => ()
                case tailNode: Node =>
                  n.next = Option(tailNode)
              }
            }
            inner(queue.tail ::: List(n.left, n.right))
        }

      }
    }
    inner(List(tree))
  }
}
