package net.koseburak.fun

object MaximumLevelSum {

  sealed trait Tree
  case object Nil extends Tree
  case class Node(value: Int, left: Tree = Nil, right: Tree = Nil) extends Tree

  def findMaxLevelSum(tree: Tree): Int = {
    import scala.collection.immutable.Queue
    def inner(queue: Queue[Tree], maxSoFar: Int): Int =
      if (queue.isEmpty) {
        maxSoFar
      } else {
        val count = queue.size
        println(count)
        val (q, sum) = (0 until count).foldLeft((queue, 0)) {
          case ((q, sum), c) =>
            val (elem, newQueue) = q.dequeue
            elem match {
              case Nil => (newQueue, sum)
              case Node(v, left, right) => (q.enqueue(left).enqueue(right), sum + v)
            }
        }
        inner(q, Math.max(sum, maxSoFar))
      }

    inner(Queue(tree), Int.MinValue)
  }

  val tree = Node(1, Node(2, Node(4), Node(5)), Node(3, Nil, Node(8, Node(6), Node(7))))

  findMaxLevelSum(tree)
}
