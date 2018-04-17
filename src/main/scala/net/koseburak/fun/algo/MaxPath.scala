package net.koseburak.fun.algo

object MaxPath {

  sealed trait Tree
  case class Node(value: Int, left: Tree, right: Tree) extends Tree
  case object Nil extends Tree

  val tree = Node(
    10,
    Node(2, Node(20, Nil, Nil), Node(1, Nil, Nil)),
    Node(10, Nil, Node(-25, Node(3, Nil, Nil), Node(4, Nil, Nil))))

  def maxPath(tree: Tree): Int = {
    var res = Int.MinValue
    def inner(l1: Tree): Int = {
      l1 match {
        case Nil => 0
        case Node(value, l, r) =>
          val leftMax = inner(l)
          val rightMax = inner(r)
          val singleMax = Math.max(Math.max(leftMax, rightMax) + value, value)
          val maxTop = Math.max(singleMax, leftMax + rightMax + value)
          res = Math.max(res, maxTop)
          singleMax
      }
    }
    inner(tree)
    res
  }

  maxPath(tree)

}
