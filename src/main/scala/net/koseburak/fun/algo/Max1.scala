package net.koseburak.fun.algo

object Max1 {

  import scala.annotation.tailrec
  // logN
  def first(arr: Array[Int]): Int = {
    @tailrec
    def inner(l: Int, r: Int): Int = {
      if (l > r)
        -1
      else {
        val mid = l + (r - l) / 2
        if ((mid == 0 || arr(mid - 1) == 0) && arr(mid) == 1)
          mid
        else if (arr(mid) == 0)
          inner(mid + 1, r)
        else inner(l, mid - 1)
      }
    }
    inner(0, arr.size - 1)
  }

  def find(arr: Array[Array[Int]]): Int = {
    val maybeFound = first(arr(0))
    println(maybeFound)
    val j = if (maybeFound != -1) maybeFound else arr(0).size - 1
    val (_, max) = arr.indices.drop(1).foldLeft((j, 0)) {
      case ((j, maxRowIndex), i) =>
        var innerJ = j
        var innerMax = maxRowIndex
        while (innerJ >= 0 && arr(i)(innerJ) == 1) {
          innerJ = innerJ - 1
          innerMax = i
        }
        (innerJ, innerMax)
    }
    max
  }

  def find2(arr: Array[Array[Int]]): Int = {
    arr.zipWithIndex.map { case (row, i) => println(i) ; (i, first(row)) }.filter(_._2 >= 0).minBy(_._2)._1
  }

  val arr = Array(Array(0, 0, 0, 1),
                  Array(0, 1, 1, 1),
                  Array(1, 1, 1, 1),
                  Array(0, 0, 0, 0))

  find(arr)

}
