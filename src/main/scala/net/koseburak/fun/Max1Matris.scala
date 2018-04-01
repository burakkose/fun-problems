package net.koseburak.fun

object Max1Matris {

  val matrix = Array(Array(0, 1, 1, 0, 1),
                     Array(1, 1, 0, 1, 0),
                     Array(0, 1, 1, 1, 0),
                     Array(1, 1, 1, 1, 0),
                     Array(1, 1, 1, 1, 1),
                     Array(0, 0, 0, 0, 0))

  def print(matrix: Array[Array[Int]]): String = {
    val solve = Array.ofDim[Int](matrix.length, matrix(0).length)
    solve(0) = matrix(0)
    solve.indices.foreach(i => solve(i)(0) = matrix(i)(0))

    val max = solve.indices.drop(1).flatMap { i =>
      solve(0).indices.drop(1).map { j =>
        if (matrix(i)(j) == 1)
          solve(i)(j) = Math.min(
            solve(i)(j - 1),
            Math.min(solve(i - 1)(j), solve(i - 1)(j - 1))) + 1
        solve(i)(j)
      }
    }.max

    List.fill(max)(List.fill(max)("1").mkString(" ")).mkString("\n")
  }

  println(matrix)

}
