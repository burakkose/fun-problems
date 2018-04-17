package net.koseburak.fun.algo

object MirrowTree {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def mirror(tree: Tree): Tree = {
    tree match {
      case Nil => Nil
      case Node(value, left, right) =>
        Node(value, mirror(right), mirror(left))
    }
  }

  def inorder(tree: Tree): Unit = {
    tree match {
      case Node(value, left, right) =>
        inorder(left)
        print(value + " ")
        inorder(right)
      case _ => ()
    }
  }

  def isMirror(tree1: Tree, tree2: Tree): Boolean = {
    (tree1, tree2) match {
      case (Nil, Nil) => true
      case (Node(d1, l1, r1), Node(d2, l2, r2)) if d1 == d2 =>
        isMirror(l1,r2) && isMirror(r1, l2)
      case _ => false
    }
  }

  val tree = Node(1, Node(2, Node(4), Node(5)), Node(3))

  inorder(tree)
  println("")
  inorder(mirror(tree))

isMirror(tree, mirror(tree))
}
