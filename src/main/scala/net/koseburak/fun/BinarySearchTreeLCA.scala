package net.koseburak.fun

import scala.annotation.tailrec

class BinarySearchTreeLCA {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree


  def lca(tree: Tree, n1: Int, n2: Int): Tree = {
    @tailrec
    def inner(t: Tree): Tree = {
      t match {
        case Nil => Nil
        case Node(data, l, r) =>
          if(data > n1 && data > n2)
            inner(l)
          else if(data < n1 && data < n2)
            inner(r)
          else t
      }
    }
    inner(tree)
  }

  val tree =
    Node(20,
      Node(8, Node(4), Node(12, Node(10), Node(14))),
      Node(22))

  lca(tree, 14,8)

}
