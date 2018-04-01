package net.koseburak.fun

object DifferenceLevelTree {

  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def difference(root: Tree): Int = {
    def inner(t: Tree): Int = {
      t match {
        case Node(d, l, r) =>
          d - inner(l) - inner(r)
        case Nil => 0
      }
    }
    inner(root)
  }

  val tree = Node(5,
                  Node(2, Node(1), Node(4, Node(3))),
                  Node(6, Nil, Node(8, Node(7), Node(9))))

  difference(tree)

  /*
      5     5 - ( 2 - ( 1 + (4 - 3 - 0) ) )
    /   \
   2     6
 /  \     \
1    4     8
    /     / \
   3     7   9
   */



}
