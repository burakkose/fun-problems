package net.koseburak.fun.algo

object BooleanMatrix {
  val arr = Array(Array(1, 0, 0, 1), Array(0, 0, 1, 0), Array(0, 0, 0, 0))

  def modify(arr: Array[Array[Int]]): Unit = {
    import scala.collection.mutable
    val rowSet = mutable.HashSet[Int]()
    val colSet = mutable.HashSet[Int]()
    for {
      i <- arr.indices
      j <- arr.head.indices
    } if (arr(i)(j) == 1) {
      rowSet += i
      colSet += j
    }
    for {
      i <- arr.indices
      j <- arr.head.indices
    } if (rowSet.contains(i) || colSet.contains(j)) {
      arr(i)(j) = 1
    }
  }

  modify(arr)

}
