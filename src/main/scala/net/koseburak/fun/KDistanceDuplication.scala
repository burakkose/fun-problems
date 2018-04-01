package net.koseburak.fun

object KDistanceDuplication {
  val arr = Array(1, 2, 3, 1, 4, 5)

  def check(arr: Array[Int], k: Int): Boolean = {
    def inner(i: Int, set: Set[Int]): Boolean = {
      if (i >= arr.length)
        false
      else if (set.contains(arr(i)))
        true
      else {
        val newSet = set + arr(i)
        inner(i + 1, if (i < k) newSet else newSet - arr(i - k))
      }
    }
    inner(0, Set.empty[Int])
  }

  check(arr, 3)
}
