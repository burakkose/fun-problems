package net.koseburak.fun.algo

object TreeToDLL {

  sealed trait Tree
  case class Node(value: Int, var left: Tree = Nil, var right: Tree = Nil)
      extends Tree
  case object Nil extends Tree

  def convert(tree: Tree): Tree = {
    def inner(root: Tree, head: Tree): Tree = {
      root match {
        case Nil =>
          println("fucka")
          head
        case n: Node =>
          val headFromRight = inner(n.right, head)
          n.right = headFromRight
          headFromRight match {
            case Nil => ()
            case hn: Node =>
              hn.left = root
          }
          inner(n.left, root)
      }
    }
    inner(tree, Nil)
  }

  def print(head: Tree): Unit = {
    head match {
      case Nil => println("Nil")
      case n: Node =>
        println(n.value)
        print(n.right)
    }
  }

  print(convert(Node(10, Node(12, Node(25), Node(30)), Node(15, Node(36)))))

}
