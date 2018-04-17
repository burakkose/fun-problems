package net.koseburak.fun.algo

object LeftView {
  sealed trait Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree
  case object Nil extends Tree

  def treePrint(tree: Tree): Unit = {
    def inner(t: Tree, currentLevel: Int, maxLevel: Int): Int = {
      t match {
        case Nil => maxLevel
        case Node(data, l, r) =>
          val newMaxLevel = if (maxLevel < currentLevel) {
            print(data + " ")
            currentLevel
          } else maxLevel
          val maxLevelAfterLeft = inner(l, currentLevel + 1, newMaxLevel)
          inner(r, currentLevel + 1, maxLevelAfterLeft)
      }
    }
    inner(tree, 1, 0)
  }

  val tree = Node(12, Node(10), Node(30, Node(25), Node(40)))

  treePrint(tree)

}
