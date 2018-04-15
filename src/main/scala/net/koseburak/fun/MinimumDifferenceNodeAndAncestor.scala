package net.koseburak.fun
// https://www.geeksforgeeks.org/maximum-difference-between-node-and-its-ancestor-in-binary-tree/
object MinimumDifferenceNodeAndAncestor {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def find(tree: Tree): Int = {
    var res = Int.MinValue
    def inner(t: Tree): Int =
      t match {
        case Nil => Int.MaxValue
        case Node(v, Nil, Nil) => v
        case Node(v, l, r) =>
          val minFromChildren = Math.min(inner(l), inner(r))
          res = Math.max(res, v - minFromChildren)
          Math.min(minFromChildren, v)
      }
    inner(tree)
    res
  }

  val tree = Node(8, Node(3, Node(1), Node(6, Node(4), Node(7))), Node(10, Nil, Node(14, Node(13))))

  find(tree)
}
