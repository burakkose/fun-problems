package net.koseburak.fun

object RotateArray {
  import scala.annotation.tailrec
  val arr = Array(1, 2, 3, 4, 5, 6, 7)
  val d = 2

  def rotate(arr: Array[Int], d: Int): Unit = {
    @tailrec
    def reverse(start: Int, end: Int): Unit = {
      if(start >= end)
        ()
      else {
        val temp = arr(start)
        arr(start) = arr(end - 1)
        arr(end - 1) = temp
        reverse(start + 1, end - 1)
      }
    }

    reverse(0, d)
    reverse(d, arr.length)
    reverse(0, arr.length)
  }

  rotate(arr, d)

  /*
  d = 2
  arr = 1, 2, 3, 4, 5, 6, 7

  1 ->
   2, 1, 3, 4, 5, 6, 7
  2 ->
   2, 1, 7, 6, 5 , 4, 3
  3 ->
   3, 4, 5, 6, 7, 1, 2
   */
}
