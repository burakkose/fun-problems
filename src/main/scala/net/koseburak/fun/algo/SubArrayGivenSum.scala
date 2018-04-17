package net.koseburak.fun.algo

object SubArrayGivenSum {

  // https://www.geeksforgeeks.org/find-subarray-with-given-sum-in-array-of-integers/

  def find(list: List[Int], target: Int): Option[(Int, Int)] = {
    import scala.annotation.tailrec
    @tailrec
    def inner(sumMap: Map[Int, Int], l: List[Int], currentSum: Int, idx: Int): Option[(Int, Int)] = {
      l match {
        case x :: xs =>
          val newCurrentSum = currentSum + x
          if (newCurrentSum == target)
            Some((0, idx))
          else if (sumMap.contains(newCurrentSum - target))
            Some((sumMap(newCurrentSum - target) + 1, idx))
          else inner(sumMap + (newCurrentSum -> idx), xs, newCurrentSum, idx + 1)
        case _ => None
      }
    }
    inner(Map(), list, 0, 0)
  }

  val list = List(1, 4, 20, 3, 10, 5)
  val target = 33

  find(list, target)

  val list2 = List(-10, 0, 2, -2, -20, 10)
  val target2 = 20

  find(list2, target2)
}
