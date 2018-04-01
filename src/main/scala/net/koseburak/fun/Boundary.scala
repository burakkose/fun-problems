package net.koseburak.fun

object Boundary {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def print(tree: Tree): Unit = {
    def leafs(t: Tree): Unit = {
      t match {
        case Nil => ()
        case Node(data, Nil, Nil) =>
          println(data)
        case Node(_, l, r) =>
          leafs(l)
          leafs(r)
      }
    }
    def left(t: Tree): Unit = {
      t match {
        case Node(data, l: Node, _) =>
          println(data)
          left(l)
        case Node(data, _, r: Node) =>
          println(data)
          left(r)
        case _ => ()
      }
    }
    def right(t: Tree): Unit = {
      t match {
        case Node(data, _, r: Node) =>
          println(data)
          right(r)
        case Node(data, l: Node, _) =>
          println(data)
          right(l)
        case _ => ()
      }
    }
    tree match {
      case Nil => ()
      case Node(data, l, r) =>
        println(data)
        left(l)
        leafs(l)
        leafs(r)
        right(r)
    }
  }

  val tree =
    Node(20,
         Node(8, Node(4), Node(12, Node(10), Node(14))),
         Node(22))

  print(tree)
}
