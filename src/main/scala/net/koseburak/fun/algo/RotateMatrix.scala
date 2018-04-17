package net.koseburak.fun.algo

object RotateMatrix {
  /*
      1 2
      3 4

      2 4
      1 3

      1 3
      2 4

      1. transpose
      2. reverseColumn
   */

  def transpose(arr: Array[Array[Int]]) = {
    for {
      i <- arr.indices
      j <- i until arr.head.length
    } yield {
      val temp = arr(j)(i)
      arr(j)(i) = arr(i)(j)
      arr(i)(j) = temp
    }
  }

  def reverseColumn(arr: Array[Array[Int]]): Unit = {
    val columnSize = arr.head.length
    val rowSize = arr.length
    for {
      currentColum <- 0 until columnSize
    } yield {
      var l = 0
      var r = rowSize - 1
      while (l < r) {
        val temp = arr(l)(currentColum)
        arr(l)(currentColum) = arr(r)(currentColum)
        arr(r)(currentColum) = temp
        l += 1
        r -= 1
      }
    }
  }

  def rotate(arr: Array[Array[Int]]): Unit = {
    transpose(arr)
    reverseColumn(arr)
  }

  val arr = Array(Array(1,2,3,4), Array(5,6,7,8), Array(9,10,11,12), Array(13,14,15,16))

  rotate(arr)
}
