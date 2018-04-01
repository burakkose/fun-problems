package net.koseburak.fun

object MaxIndexDiff {
  //https://www.geeksforgeeks.org/given-an-array-arr-find-the-maximum-j-i-such-that-arrj-arri/

  def find(arr: Array[Int]): Int = {
    import scala.annotation.tailrec
    val size = arr.length

    val lMin = arr.tail.scanLeft(arr.head)(Math.min)
    val rMax = arr.dropRight(1).scanRight(arr.last)(Math.max)

    @tailrec
    def inner(i: Int, j: Int, result: Int): Int = {
      if (i < size && j < size) {
        if (lMin(i) < rMax(j))
          inner(i, j + 1, Math.max(j - i, result))
        else inner(i + 1, j, result)
      } else {
        result
      }
    }

    inner(0, 0, -1)
  }

  val arr = Array(9, 2, 3, 4, 5, 6, 7, 8, 18, 0)

  find(arr)

}
