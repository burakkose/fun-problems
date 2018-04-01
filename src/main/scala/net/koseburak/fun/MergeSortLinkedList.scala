package net.koseburak.fun

object MergeSortLinkedList {
  sealed trait List
  case class Cons(data: Int, tail: List = Nil) extends List
  case object Nil extends List

  def sort(list: List): List = {
    def split(l: List): (List, List) = {
      def inner(fast: List, slow: List, acc: List): (List, List) = {
        (slow, fast) match {
          case (Cons(v, slowTail), Cons(_, Cons(_, newFast))) =>
            inner(newFast, slowTail, Cons(v, acc))
          case _ => (acc, slow)
        }
      }
      inner(list, list, Nil)
    }

    def merge(l1: List, l2: List): List = {
      (l1, l2) match {
        case (Nil, _) => l2
        case (_, Nil) => l1
        case (Cons(firstH, firstTail), Cons(secondH, secondTail)) =>
          if (firstH < secondH) Cons(firstH, merge(firstTail, l2))
          else Cons(secondH, merge(l1, secondTail))
      }
    }

    list match {
      case Nil          => Nil
      case Cons(_, Nil) => list
      case _ =>
        val (left, right) = split(list)
        merge(sort(left), sort(right))
    }
  }

  val list = Cons(5, Cons(4, Cons(3, Cons(2, Cons(1)))))
  sort(list)
}
