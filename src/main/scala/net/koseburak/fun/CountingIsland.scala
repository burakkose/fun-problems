package net.koseburak.fun

object CountingIsland {

  def count(matrix: Array[Array[Int]]): Int = {
    import scala.collection.mutable
    val row = matrix.length
    val column = matrix.head.length
    val visited = mutable.HashSet[(Int, Int)]()
    val position = List(-1, 0, 1)
    val neighbors =
      for (p1 <- position; p2 <- position if p1 != p2 || p1 != 0 && p2 != 0)
        yield (p1, p2)

    def isSafe(i: Int, j: Int): Boolean = {
      i >= 0 && i < row && j >= 0 && j < column && matrix(i)(j) == 1 && !visited
        .contains((i, j))
    }

    def dfs(i: Int, j: Int): Unit = {
      visited += ((i, j))
      neighbors.foreach {
        case (ni, nj) =>
          val newI = i + ni
          val newJ = j + nj
          if (isSafe(newI, newJ))
            dfs(newI, newJ)
      }
    }

    var count = 0
    for {
      i <- 0 until row
      j <- 0 until column
    } if (isSafe(i, j)) {
      dfs(i, j)
      count += 1
    }

    count

  }

  val matrix = Array(Array(1, 1, 0, 0, 0),
                     Array(0, 1, 0, 0, 1),
                     Array(1, 0, 0, 1, 1),
                     Array(0, 0, 0, 0, 0),
                     Array(1, 0, 1, 0, 1))

  count(matrix)
}
