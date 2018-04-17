package net.koseburak.fun.algo

object DiameterPath {
  sealed trait Tree
  case class Node(v: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def diameter(tree: Tree): Int = {
    var ans = 0
    def inner(t: Tree): Int = {
      t match {
        case Node(_, l, r) =>
          val lh = inner(l)
          val rh = inner(r)
          ans = Math.max(ans, lh + rh + 1)
          1 + Math.max(lh, rh)
        case Nil => 0
      }
    }
    inner(tree)
    ans
  }

  val tree = Node(1, Node(1, Node(1, Node(1), Node(1, Node(1, Nil, Node(1)))), Node(1, Nil, Node(1, Node(1), Node(1, Node(1))))), Node(1))

  diameter(tree)
}
