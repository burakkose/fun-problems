package net.koseburak.fun.algo

object DifferencePair {
  import scala.annotation.tailrec
  val arr = Array(-59, 90, 1, 8, 60, 30, 40, 100, 120)

  def find(arr: Array[Int], k: Int): List[(Int, Int)] = {
    val length = arr.length
    @tailrec
    def inner(idx: Int, solutions: List[(Int, Int)], set: Set[Int]): List[(Int, Int)] =
      if (idx < length) {
        val elem = arr(idx)
        val need1 = elem + k
        val need2 = elem - k
        val newSet =
          if (elem > k) Set(need1, need2, k - elem) else Set(need1, need2)
        if (set.contains(elem)) {
          val sol = if (elem > k) elem - k else elem + k
          inner(idx + 1, solutions :+ (sol, elem), set ++ newSet)
        } else
          inner(idx + 1, solutions, set ++ newSet)
      } else solutions
    inner(0, List.empty[(Int, Int)], Set.empty[Int])
  }

  find(arr, 60)
}
