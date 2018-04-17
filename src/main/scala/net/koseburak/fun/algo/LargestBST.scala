package net.koseburak.fun.algo

object LargestBST {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  case class Info(size: Int, max: Int, min: Int, answer: Int, isBST: Boolean)

  def largestBST(tree: Tree): Info = {
    tree match {
      case Nil               => Info(0, Int.MinValue, Int.MaxValue, 0, true)
      case Node(d, Nil, Nil) => Info(1, d, d, 1, true)
      case Node(d, l, r) =>
        val leftInfo = largestBST(l)
        val rightInfo = largestBST(r)
        val size = leftInfo.size + rightInfo.size + 1

        if (leftInfo.isBST && rightInfo.isBST && leftInfo.max < d && rightInfo.min > d) {
          val min = List(leftInfo.min, rightInfo.min, d).min
          val max = List(leftInfo.max, rightInfo.max, d).max
          val answer = size
          val isBST = true
          Info(size, max, min, answer, isBST)
        } else {
          Info(size, -1, -1, Math.max(leftInfo.answer, rightInfo.answer), false)
        }
    }
  }

  val tree = Node(60, Node(65, Node(50)), Node(70))
  println(largestBST(tree))
}
