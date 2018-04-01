package net.koseburak.fun

object LeaderInArray {
  val arr = Array(16, 17, 4, 3, 5, 2)

  def findLeaders(arr: Array[Int]): Seq[Int] = {
    var maxFromRight = Int.MinValue
    for(i <- arr.length - 1 to 0 by -1 if arr(i) > maxFromRight) yield {
      maxFromRight = arr(i)
      maxFromRight
    }
  }

  def recursive(arr: Array[Int]): Seq[Int] = {
    import scala.annotation.tailrec
    @tailrec
    def inner(i: Int, max: Int, acc: List[Int]): List[Int] = {
      if (i < 0)
        acc
      else {
        val (nMax, nAcc) = if(arr(i) > max) (arr(i), arr(i) :: acc) else (max, acc)
        inner(i - 1, nMax, nAcc)
      }
    }
    inner(arr.length - 1, Int.MinValue, List.empty[Int])
  }

  recursive(arr)
}
