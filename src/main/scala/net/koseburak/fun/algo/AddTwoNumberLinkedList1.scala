package net.koseburak.fun.algo

object AddTwoNumberLinkedList1 {
  sealed trait List
  case class Cons(value: Int, tail: List) extends List
  case object Nil extends List

  /*
    First List: 7->5->9->4->6  // represents number 64957
    Second List: 8->4 //  represents number 48
   */
  def add(first: List, second: List): List = {
    def inner(l1: List, l2: List, carry: Int): List = {
      (l1, l2) match {
        case (Nil, Nil) => Nil
        case (Nil, Cons(d, tail)) =>
          val sum = d + carry
          val newCarry = sum / 10
          val newSum = sum % 10
          Cons(newSum, inner(Nil, tail, newCarry))
        case (Cons(d, tail), Nil) =>
          val sum = d + carry
          val newCarry = sum / 10
          val newSum = sum % 10
          Cons(newSum, inner(Nil, tail, newCarry))
        case (Cons(d1, tail1), Cons(d2, tail2)) =>
          val sum = d1 + d2 + carry
          val newCarry = sum / 10
          val newSum = sum % 10
          Cons(newSum, inner(tail1, tail2, newCarry))
      }
    }
    inner(first, second, 0)
  }

  val f1 = Cons(7, Cons(5, Cons(9, Cons(4, Cons(6, Nil)))))
  val f2 = Cons(8, Cons(4, Nil))

  add(f1,f2)
}
