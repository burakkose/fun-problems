package net.koseburak.fun.algo

object MergeInterval {

  val a = Array((6, 8), (1, 9), (2, 4), (4, 7))

  def merge(a: Array[(Int, Int)]): List[(Int, Int)] = {
    val n = a.size
    val sorted = a.sortBy(_._1)

    def inner(stack: List[(Int, Int)], idx: Int): List[(Int, Int)] = {
      if (idx == n)
        stack
      else {
        val top = stack.head
        if (top._2 < sorted(idx)._1) {
          inner(sorted(idx) :: stack, idx + 1)
        } else if (top._2 < sorted(idx)._2) {
          inner((top._1, sorted(idx)._2) :: stack.tail, idx + 1)
        } else inner(stack, idx + 1)
      }
    }

    inner(List(sorted(0)), 1)
  }

  merge(a)

}
