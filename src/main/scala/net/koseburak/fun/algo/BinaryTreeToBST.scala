package net.koseburak.fun.algo

object BinaryTreeToBST {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def btToBst(root: Tree): Tree = {
    def inorderArr(t: Tree): List[Int] = {
      t match {
        case Nil => List.empty[Int]
        case Node(v, l, r) =>
          inorderArr(l) ::: List(v) ::: inorderArr(r)
      }
    }
    def applyInOrder(root: Tree, list: List[Int]): (Tree, List[Int]) = {
      root match {
        case Nil => (Nil, list)
        case Node(d, l, r) =>
          val (lTree, lList) = applyInOrder(l, list)
          val newData = lList.head
          val (rTree, rList) = applyInOrder(r, lList.tail)
          (Node(newData, lTree, rTree), rList)
      }
    }
    val result = applyInOrder(root, inorderArr(root).sorted)._1
    println(inorderArr(result))
    result
  }

  val root = Node(10, Node(30, Node(20)), Node(15, Nil, Node(5)))

  btToBst(root)

}
