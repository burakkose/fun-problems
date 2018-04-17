package net.koseburak.fun.algo

object MinCostPath {

  def minCost(matrix: Array[Array[Int]]): (Int, Array[Array[Int]]) = {
    val row = matrix.length
    val column = matrix.head.length
    val cost = Array.ofDim[Int](row, column)
    cost(0)(0) = matrix.head.head
    (1 until row).foreach { index =>
      cost(index)(0) = cost(index - 1)(0) + matrix(index)(0)
    }
    (1 until column).foreach { index =>
      cost(0)(index) = cost(0)(index - 1) + matrix(0)(index)
    }

    for{
      i <- 1 until row
      j <- 1 until column
    } {
      cost(i)(j) = List(cost(i - 1)(j - 1), cost(i)(j-1), cost(i-1)(j)).min + matrix(i)(j)
    }

    (cost(row - 1)(column - 1), cost)
  }

  val matrix = Array(Array(1, 2, 3), Array(4, 8, 2), Array(1, 5, 3))

  minCost(matrix)

}
