package net.koseburak.fun.algo

object CountingSortLinkedList {

  sealed trait List
  case class Cons(data: Int, tail: List = Nil) extends List
  case object Nil extends List

  val numbers = Array(5, 1, 3, 2, 8)
  val list = Cons(3, Cons(2, Cons(5, Cons(8, Cons(6, Cons(2, Cons(1)))))))

  def sort(list: List, numbers: Array[Int]): List = {
    val (min, max) = numbers.foldLeft((Int.MaxValue, Int.MinValue)) {
      case ((min, max), elem) => (Math.min(min, elem), Math.max(max, elem))
    }

    val size = (max - min) + 1
    val countingArr = Array.fill(size)(0)
    import scala.annotation.tailrec
    @tailrec
    def count(l: List): Unit = {
      l match {
        case Cons(d, tail) =>
          countingArr(d - min) += 1
          count(tail)
        case _ => ()
      }
    }

    count(list)

    countingArr.zipWithIndex.foldRight[List](Nil) {
      case ((count, index), acc) =>
        if (count > 0) {
          (0 until count).foldLeft[List](acc) {
            case (acc, _) =>
              Cons(min + index, acc)
          }
        } else acc
    }

  }

  sort(list, numbers)

}
