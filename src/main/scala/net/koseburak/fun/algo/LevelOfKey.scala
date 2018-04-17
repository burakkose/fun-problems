package net.koseburak.fun.algo

object LevelOfKey {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree


  def findLevel(tree: Tree, key: Int): Int = {
    def inner(t: Tree, level: Int): Option[Int] = {
      t match {
        case Node(v, l, r) =>
          if(v == key)
            Some(level)
          else
            inner(l, level + 1).orElse(inner(r, level + 1))
        case Nil => None
      }
    }
    inner(tree, 1).getOrElse(0)
  }

  val tree =Node(3, Node(2, Node(1), Node(4)), Node(5))

  findLevel(tree, 1)

}
