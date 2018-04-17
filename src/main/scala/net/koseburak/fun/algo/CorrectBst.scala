package net.koseburak.fun.algo

object CorrectBst {
  sealed trait Tree
  case class Node(var value: Int, left: Tree = Nil, right: Tree = Nil)
      extends Tree
  case object Nil extends Tree

  def correct(tree: Tree): Unit = {
    var first: Option[Node] = None
    var last: Option[Node] = None
    var prev: Option[Node] = None
    def inner(node: Tree): Unit = {
      node match {
        case n: Node =>
          inner(n.left)
          prev
            .filter(pd => n.value < pd.value)
            .foreach(
              _ =>
                first
                  .map(_ => last = Some(n))
                  .getOrElse {
                    first = prev
                    last = Some(n)
                })
          prev = Some(n)
          inner(n.right)
        case _ => ()
      }
    }
    inner(tree)
    (first zip last).headOption
      .foreach {
        case (f, m) =>
          val temp = f.value
          f.value = m.value
          m.value = temp
      }
  }

  val tree = Node(6, Node(10, Node(1), Node(3)), Node(2, Node(7), Node(12)))
  println(tree)
  correct(tree)
  println(tree)

}
