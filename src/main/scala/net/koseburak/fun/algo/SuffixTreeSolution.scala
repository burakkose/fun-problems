package net.koseburak.fun.algo

object SuffixTreeSolution {
  import scala.annotation.tailrec
  import scala.collection.mutable
  import scala.collection.mutable.ListBuffer
  case class SuffixTree private (
      indexes: mutable.ListBuffer[Int] = ListBuffer.empty[Int],
      children: mutable.HashMap[Char, SuffixTree] = mutable.HashMap.empty
  ) {

    def insertSuffix(suffix: String, index: Int): Unit = {
      @tailrec
      def inner(n: SuffixTree, s: String, i: Int): Unit = {
        n.indexes += index
        if (s.length > 0) {
          val h = s.head
          n.children.update(h, n.children.getOrElse(h, SuffixTree()))
          inner(n.children(h), s.tail, index + 1)
        }
      }
      inner(this, suffix, index)
    }

    def search(query: String): List[Int] = {
      def inner(node: SuffixTree, q: String): List[Int] = {
        if (q.isEmpty)
          node.indexes.toList
        else if (children.contains(query.head))
          node.children(q.head).search(q.tail)
        else List.empty[Int]
      }
      inner(this, query)
    }
  }

  object SuffixTree {
    def create(text: String): SuffixTree = {
      val root = SuffixTree()
      text.tails.map(_ + "$").zipWithIndex.foreach {
        case (suffix, index) => root.insertSuffix(suffix, index)
      }
      root
    }
  }

  val text = "geeksforgeeks.org"
  val root = SuffixTree.create(text)
  root.search("geek")
}
