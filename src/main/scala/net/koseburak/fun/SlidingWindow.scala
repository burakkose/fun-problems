package net.koseburak.fun

// https://www.geeksforgeeks.org/sliding-window-maximum-maximum-of-all-subarrays-of-size-k/
object SlidingWindow {
  // because scala is not happy with DoubleLinkedList (deprecated)
  import scala.annotation.tailrec
  import java.util
  import scala.collection.mutable.ListBuffer
  def find(arr: Array[Int], k: Int): List[Int] = {
    def removeSmallerElements(w: util.LinkedList[Int], i: Int): Unit =
      while (!w.isEmpty && arr(i) >= arr(w.peekLast())) {
        w.removeLast()
      }
    def fixWindow(w: util.LinkedList[Int], i: Int): Unit =
      while (!w.isEmpty && w.peek() <= i - k) w.removeFirst()
    @tailrec
    def inner(i: Int, w: util.LinkedList[Int], res: ListBuffer[Int]): List[Int] =
      if (i >= arr.length) {
        res += arr(w.peek)
        res.toList
      } else if (i < k) {
        removeSmallerElements(w, i)
        w.addLast(i)
        inner(i + 1, w, res)
      } else {
        res += arr(w.peek) // from previous
        fixWindow(w, i)
        removeSmallerElements(w, i)
        w.addLast(i)
        inner(i + 1, w, res)
      }

    inner(0, new util.LinkedList[Int](), ListBuffer[Int]())
  }

  val arr = Array(12, 1, 78, 90, 57, 89, 56)
  find(arr, 3)
}
