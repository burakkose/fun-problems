package net.koseburak.fun.algo

object AlienLanguage {

  import scala.collection.mutable
  import scala.collection.mutable.{ListBuffer, Map}

  case class Graph private(private val adj: Map[Char, ListBuffer[Char]] = Map.empty) {
    def addEdge(v1: Char, v2: Char): Unit = {
      adj(v1) += v2
    }

    def topologicalSort: List[Char] = {
      val visited = mutable.HashSet.empty[Char]
      val stack = mutable.Stack[Char]()
      val pushed = mutable.HashSet.empty[Char]

      def inner(ch: Char, adjList: List[Char]): Unit = {
        visited += ch
        pushed += ch
        adjList.foreach { char =>
          if (pushed.contains(char))
            throw new IllegalArgumentException("cyclic")
          if (!visited.contains(char)) inner(char, adj(char).toList)
        }
        stack.push(ch)
      }

      adj.foreach { case (ch, adj) =>
        if (!visited.contains(ch))
          inner(ch, adj.toList)
      }

      stack.toList
    }
  }

  object Graph {
    def apply(set: Set[Char]): Graph = new Graph(Map.empty[Char, ListBuffer[Char]] ++ set.map(_ -> ListBuffer.empty[Char]).toMap)
  }

  def findOrder(wordList: List[String]): List[Char] = {
    val alphabet = wordList.foldLeft(Set.empty[Char])((set, word) => set ++ word.toList)
    val graph = Graph(alphabet)
    wordList.tail.foldLeft(wordList.head) { case (word1, word2) =>
      def constructEdges(i: Int, limit: Int): Unit = {
        if (i < limit) {
          if (word1.charAt(i) == word2.charAt(i)) constructEdges(i + 1, limit)
          else {
            graph.addEdge(word1.charAt(i), word2.charAt(i))
            ()
          }
        } else ()
      }

      constructEdges(0, Math.min(word1.length, word2.length))
      word2
    }
    graph.topologicalSort
  }

  val l = List("aba", "bba", "aaa")
  findOrder(l)

}
