package net.koseburak.fun

object MedianOfStream {
  import scala.annotation.tailrec
  import scala.collection.mutable
  def findMedian(list: List[Int]): List[Int] = {
    val leftHeap = mutable.PriorityQueue[Int]()
    val rightHeap = mutable.PriorityQueue[Int]()(Ordering.by[Int, Int](-_))
    @tailrec
    def inner(l: List[Int], acc: List[Int]): List[Int] =
      l match {
        case h :: t =>
          if (leftHeap.size > rightHeap.size) {
            if (h < acc.head) {
              rightHeap enqueue leftHeap.dequeue()
              leftHeap enqueue h
            } else
              rightHeap enqueue h
            inner(t, (leftHeap.head + rightHeap.head) / 2 :: acc)
          } else if (leftHeap.size < rightHeap.size) {
            if (h < acc.head) {
              leftHeap enqueue h
            } else {
              leftHeap enqueue rightHeap.dequeue()
              rightHeap enqueue h
            }

            inner(t, (leftHeap.head + rightHeap.head) / 2 :: acc)
          } else {
            if (h < acc.head) {
              leftHeap enqueue h
              inner(t, leftHeap.head :: acc)
            } else {
              rightHeap enqueue h
              inner(t, rightHeap.head :: acc)
            }
          }
        case _ => acc.reverse.tail
      }
    inner(list, List(0))
  }

  val l = List(5, 15, 1, 3, 2, 8, 7, 9, 10, 6, 11, 4)

  findMedian(l)
}
