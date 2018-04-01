package net.koseburak.fun

object SortFrequency {

  val arr = Array(2, 3, 2, 4, 5, 12, 2, 3, 3, 3, 12)

  def sortByFrequency(arr: Array[Int]): List[Int] = {
    arr.groupBy(identity).toList.sortBy(-_._2.length).flatMap(_._2)
  }

  def sortInPlaceStableByFrequency(arr: Array[Int]): Unit = {
    val frequency = arr.groupBy(identity).mapValues(_.length)
    scala.util.Sorting.stableSort(arr, (e1: Int, e2: Int) => frequency(e1) > frequency(e2))
  }

  def sortInPlaceQuickSort(arr: Array[Int]): Unit = {
    val frequency = arr.groupBy(identity).mapValues(_.length)
    implicit val ev = Ordering.by[Int, Int](frequency(_))
    scala.util.Sorting.quickSort(arr)(ev)
  }

  sortByFrequency(arr)
}
