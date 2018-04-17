package net.koseburak.fun.algo

object SumPair {
  import scala.annotation.tailrec
  val arr = Array(1, 4, 45, 6, 10, 8)

  def find(arr: Array[Int], k: Int): List[(Int, Int)] = {
    val length = arr.length
    @tailrec
    def inner(idx: Int,
              solutions: List[(Int, Int)],
              set: Set[Int]): List[(Int, Int)] = {
      if (idx < length) {
        val elem = arr(idx)
        val remains = k - elem
        if (remains >= 0 && set.contains(remains))
          inner(idx + 1, solutions :+ (elem, remains), set + elem)
        else
          inner(idx + 1, solutions, set + elem)
      } else solutions
    }
    inner(0, List.empty[(Int, Int)], Set.empty[Int])
  }

  def find2(arr: Array[Int], k: Int): Int = {
    val map = arr.groupBy(identity).mapValues(_.length)
    arr.foldLeft(0) {
      case (count, elem) =>
        if (map.contains(k - elem)) {
          val possibleCount = count + map(k - elem)
          if (k - elem == elem)
            possibleCount - 1
          else possibleCount
        } else count
    } / 2
  }

  find2(arr, 16)

}
