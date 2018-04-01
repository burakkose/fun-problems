package net.koseburak.fun

object CheckBinaryTreeSubTree {
  sealed trait Tree
  case class Node(value: String, left: Tree = Nil, right: Tree = Nil)
      extends Tree
  case object Nil extends Tree

  def check(tree: Tree, subTree: Tree): Boolean = {
    def inorder(t: Tree): String = {
      t match {
        case Node(d, l, r) =>
          inorder(l) + d + inorder(r)
        case Nil => "$"
      }
    }

    def preOrder(t: Tree): String = {
      t match {
        case Node(d, l, r) =>
          d + preOrder(l) + preOrder(r)
        case Nil => "$"
      }
    }

    val treeInorder = inorder(tree)
    println(treeInorder)
    val subTreeInOrder = inorder(subTree)
    println(subTreeInOrder)
    if (!treeInorder.contains(subTreeInOrder))
      false
    else {
      val treePreorder = preOrder(tree)
      val subTreePreOrder = preOrder(subTree)
      treePreorder.contains(subTreePreOrder)
    }
  }

  val tree = Node("z",
                  Node("x", Node("a", Nil, Node("c")), Node("b")),
                  Node("e", Nil, Node("k")))

  val subTree = Node("x", Node("a", Nil, Node("c")), Node("b"))

  check(tree, subTree)

}
