package net.koseburak.fun.algo

object SpecialTreePreOrder {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def create(pre: List[Int], preLN: List[Char]): Tree = {
    def inner(data: List[(Int, Char)]): (Tree, List[(Int, Char)]) = {
      data match {
        case (value, id) :: tail =>
          if(id == 'L')
            (Node(value), tail)
          else {
            val (leftTree, leftList) = inner(tail)
            val (rightTree, rightList) = inner(leftList)
            (Node(value, leftTree, rightTree), rightList)
          }
        case _ => (Nil, data)
      }
    }
    inner(pre.zip(preLN))._1
  }

  def inOrder(tree: Tree): String = {
    tree match {
      case Nil => ""
      case Node(d, l , r) => inOrder(l) + d + " " + inOrder(r)
    }
  }

  inOrder(create(List(10, 30, 20, 5, 15), List('N', 'N', 'L', 'L', 'L')))
}
