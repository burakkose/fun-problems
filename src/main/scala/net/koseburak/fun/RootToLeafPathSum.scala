package net.koseburak.fun

object RootToLeafPathSum {

  sealed trait Tree
  case object Nil extends Tree
  case class Node(v: Int, left: Tree = Nil, right: Tree = Nil) extends Tree

  def hasSum(tree: Tree, sum: Int): Boolean = {
    def inner(t: Tree, innerSum: Int): Boolean =
      t match {
        case Nil => false
        case Node(v, Nil, Nil) => (innerSum + v) == sum
        case Node(v, left, right) =>
          inner(left, innerSum + v) || inner(right, innerSum + v)
      }

    inner(tree, 0)
  }

  val tree = Node(10, Node(8, Node(3), Node(5)), Node(2, Node(2)))

  hasSum(tree, 21)
}
