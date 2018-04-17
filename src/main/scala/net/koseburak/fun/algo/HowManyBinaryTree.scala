package net.koseburak.fun.algo

object HowManyBinaryTree {
  def find(n: Int): Long = {
    val arr = Array.fill(n + 1)(0L)
    arr(0) = 1
    arr(1) = 1

    (2 to n).foreach { i =>
      (0 until i).foreach { j =>
        arr(i) += (arr(j) * arr(i - j - 1))
      }
    }

    arr(n)
  }

  find(1)
}
