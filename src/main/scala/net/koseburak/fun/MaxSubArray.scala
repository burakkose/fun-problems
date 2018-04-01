package net.koseburak.fun

object MaxSubArray {
  def maxSubArraySum(arr: Array[Int]): Int = {
    arr.drop(1).foldLeft((arr(0), arr(0))) {
      case ((maxSoFar, currentMax), elem) =>
        val innerMax = Math.max(elem, currentMax + elem)
        (Math.max(maxSoFar, innerMax), innerMax)
    }._1
  }

  val arr = Array(-2, -3, 4, -1, -2, 1, 5, -3)

  maxSubArraySum(arr)
}
