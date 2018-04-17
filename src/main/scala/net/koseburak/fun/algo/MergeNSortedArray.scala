package net.koseburak.fun.algo

object MergeNSortedArray {
  var arr = Array(
    Array(2, 6, 12, 34),
    Array(1, 9, 20, 1000),
    Array(23, 34, 90, 2000)
  )

  case class Node(element: Int, i: Int, j: Int)

  def merge(arr: Array[Array[Int]]): Seq[Int] = {
    val k = arr.length
    val nodes = (0 until k).map { i =>
      Node(arr(i)(0), i, 1)
    }
    val minHeap = scala.collection.mutable
      .PriorityQueue[Node](nodes: _*)(Ordering.by[Node, Int](-_.element))

    (0 until (arr(0).length * k)).map { _ =>
      val min = minHeap.dequeue()

      val newNode =
        if (min.j < arr(0).length)
          min.copy(
            element = arr(min.i)(min.j),
            j = min.j + 1
          )
        else min.copy(element = Int.MaxValue)

      minHeap.enqueue(newNode)

      min.element
    }
  }

  merge(arr)
}
