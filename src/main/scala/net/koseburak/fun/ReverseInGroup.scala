package net.koseburak.fun

object ReverseInGroup {

  sealed trait List
  case class Cons(data: Int, var tail: List = Nil) extends List
  case object Nil extends List

  val data = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8))))))))

  def reverse(list: List, k: Int): List = {
    var current: List = list
    var next = Nil
    var prev = Nil
    var count = 0

    while(current != Nil && count < k) {
      next = current match {
        case Cons(d, tail) => tail
        case _ => Nil
      }

    }
  }

  reverse(data, 3)

}
