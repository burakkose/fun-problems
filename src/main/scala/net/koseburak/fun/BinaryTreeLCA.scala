package net.koseburak.fun

object BinaryTreeLCA {

  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def find(tree: Tree, n1: Int, n2: Int): Tree = {
    def inner(t: Tree): Tree = {
      t match {
        case Nil => Nil
        case Node(data, l, r) =>
          if (data == n1 || data == n2)
            t
          else {
            val leftLca = inner(l)
            val rightLca = inner(r)
            (leftLca, rightLca) match {
              case (Nil, Nil)         => Nil
              case (_: Node, _: Node) => t
              case (l1: Node, Nil)    => l1
              case (Nil, l2: Node)    => l2
            }
          }
      }
    }
    inner(tree)
  }

  val tree = Node(1, Node(2, Node(4), Node(5)), Node(3, Node(6), Node(7)))

  find(tree, 4, 5)
}
