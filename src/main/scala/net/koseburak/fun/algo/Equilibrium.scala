package net.koseburak.fun.algo

object Equilibrium {
  val arr = Array(-7, 1, 5, 2, -4, 3, 0)

  def find(arr: Array[Int]): Option[Int] = {
    def inner(leftSum: Int, sum: Int, idx: Int): Option[Int] = {
      if (idx > arr.size)
        None
      else {
        val newSum = sum - arr(idx)
        if (newSum == leftSum)
          Some(idx)
        else inner(leftSum + arr(idx), newSum, idx + 1)
      }
    }
    inner(0, arr.sum, 0)
  }

  find(arr)

}
