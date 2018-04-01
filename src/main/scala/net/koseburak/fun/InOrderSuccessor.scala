package net.koseburak.fun

object InOrderSuccessor {

  sealed trait Tree
  case class Node(value: Int, left: Tree= Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def inOrderSuccessor(root: Tree, node: Tree): Tree = {
    import scala.annotation.tailrec
    @tailrec
    def minValueNode(n: Tree): Tree = {
      n match {
        case n@Node(_, Nil, _) => n
        case Node(_, l, _) => minValueNode(l)
        case _ => Nil
      }
    }

    @tailrec
    def inner(node: Tree, succ: Tree, v: Int): Tree = node match {
      case Node(d, l, r) =>
        if (d < v)
          inner(l, node, v)
        else if (d > v)
          inner(r, succ, v)
        else succ
      case Nil => succ
    }

    node match {
      case Node(v, _, Nil) => inner(root, root, v)
      case Node(_, _, r) => minValueNode(r)
      case Nil => Nil
    }
  }

  val target = Node(8, Node(4), Node(12, Node(10), Node(14)))
  val tree = Node(20, target , Node(22))

  inOrderSuccessor(tree, target)
}
