package net.koseburak.fun

object SumTree {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def sumTree(tree: Tree): Tree = {
    def inner(t: Tree): (Int, Tree) =
      t match {
        case Nil => (0, Nil)
        case Node(data, l, r) =>
          val (oldLeftData, newLeft) = inner(l)
          val (oldRightData, newRight) = inner(r)
          (data + oldLeftData + oldRightData,
           Node(oldLeftData + oldRightData, newLeft, newRight))
      }
    inner(tree)._2
  }

  def inOrder(tree: Tree): String = {
    tree match {
      case Nil => ""
      case Node(data, l, r) =>
        inOrder(l) + " " + data.toString + " " + inOrder(r)
    }
  }

  val tree = Node(10, Node(-2, Node(8), Node(-4)), Node(6, Node(7), Node(5)))

  inOrder(tree)
  inOrder(sumTree(tree))
}
