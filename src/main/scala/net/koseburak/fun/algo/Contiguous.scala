package net.koseburak.fun.algo

object Contiguous {
  val arr = Array(2, 3, 6, 4, 4, 6, 6, 5)

  def solve(arr: Array[Int]): Boolean = {
    val set = arr.toSet
    def search(currentElement: Int, count: Int, op: Int => Int): Int = {
      if (set.contains(currentElement)) {
        search(op(currentElement), count + 1, op)
      } else {
        count
      }
    }
    val count = search(arr(0) + 1, search(arr(0) - 1, 1, _ - 1), _ + 1)
    println(count)
    count == set.size
  }

  solve(arr)
}
