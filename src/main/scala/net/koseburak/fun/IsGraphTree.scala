package net.koseburak.fun

object IsGraphTree {
  import scala.collection.immutable.Queue
  import scala.collection.mutable.{HashMap, HashSet}
  case class Graph(
      private val adj: HashMap[Int, HashSet[Int]] =
        HashMap.empty[Int, HashSet[Int]]) {
    def addEdge(i: Int, j: Int): Unit = {
      adj.getOrElseUpdate(i, HashSet.empty) += j
      adj.getOrElseUpdate(j, HashSet.empty) += i
    }
    def isTree: Boolean = {
      val visited = HashSet.empty[Int]
      def isCyclic(queue: Queue[Int]): Boolean = {
        println(queue)
        if (queue.isEmpty) false
        else {
          val h = queue.head
          if (visited.contains(h)) true
          else {
            visited += h
            isCyclic(queue.tail.enqueue((adj(h) - h).toList))
          }
        }
      }
      if (isCyclic(Queue(adj.head._1)) || adj.size != visited.size) {
        false
      }
      else true
    }
  }

  val graph = Graph()
  graph.addEdge(1, 0)
  graph.addEdge(0, 2)
  graph.addEdge(0, 3)
  graph.addEdge(3, 4)

  graph.isTree
}
