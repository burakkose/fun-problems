package net.koseburak.fun

object kthInBST {

  sealed trait Tree
  case object Nil extends Tree
  case class Node(v: Int, left: Tree = Nil, right: Tree = Nil) extends Tree

  def findK(tree: Tree, k: Int): Option[Int] = {
    var level = 0
    def inner(t: Tree): Option[Int] =
      t match {
        case Node(v, left, right) =>
          inner(left)
            .orElse {
              level += 1
              if (level == k) {
                Some(v)
              } else {
                inner(right)
              }
            }
        case Nil => None
      }

    inner(tree)
  }

  val tree = Node(5, Node(3, Node(2), Node(4)), Node(7, Node(6), Node(8)))
  findK(tree, 3)
}
