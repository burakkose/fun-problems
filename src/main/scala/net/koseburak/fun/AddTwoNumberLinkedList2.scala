package net.koseburak.fun

object AddTwoNumberLinkedList2 {

  sealed trait List {
    def size: Int
  }

  case class Cons(value: Int, tail: List) extends List {
    override def size: Int = 1 + tail.size
  }

  case object Nil extends List {
    override def size: Int = 0
  }

  def add(list1: List, list2: List): List = {
    def split(l: List, diff: Int): (List, List) = {
      if (diff == 0)
        (Nil, l)
      else {
        l match {
          case Cons(d, tail) =>
            val (l, r) = split(tail, diff - 1)
            (Cons(d, l), r)
          case _ => (Nil, Nil)
        }
      }
    }

    def addSameSize(l1: List, l2: List): (Int, List) = {
      (l1, l2) match {
        case (Cons(d1, tail1), Cons(d2, tail2)) =>
          val (carry, list) = addSameSize(tail1, tail2)
          val sum = carry + d1 + d2
          val newCarry = sum / 10
          val newSum = sum % 10
          (newCarry, Cons(newSum, list))
        case _ => (0, Nil)
      }
    }

    def propogateCarry(l1: List, l2: List, initialCarry: Int): (Int, List) = {
      l1 match {
        case Nil => (initialCarry, l2)
        case Cons(d, tail) =>
          val (carry, l) = propogateCarry(tail, l2, initialCarry)
          val sum = carry + d
          val newCarry = sum / 10
          val newSum = sum % 10
          (newCarry, Cons(newSum, l))
      }
    }

    val List((c1, smaller), (c2, possibleLarger)) =
      List(list1, list2).map(l => (l.size, l)).sortBy(_._1)

    if (c1 == c2) {
      val (carry, l) = addSameSize(smaller, possibleLarger)
      if (carry > 0) Cons(carry, l) else l
    } else {
      val diff = c2 - c1
      val (later, now) = split(possibleLarger, diff)
      val (carry, l) = addSameSize(now, smaller)
      val (carry1, l2) = propogateCarry(later, l, carry)
      if (carry1 > 0) Cons(carry1, l2) else l2
    }
  }

  val a = Cons(9, Cons(9, Cons(9, Nil)))
  val b = Cons(1, Cons(8, Nil))

  add(a, b)

}
