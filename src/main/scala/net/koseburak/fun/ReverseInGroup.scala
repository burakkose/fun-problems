package net.koseburak.fun

object ReverseInGroup {

  sealed trait List
  case object Nil extends List
  case class Cons(data: Int, var tail: List = Nil) extends List

  def reverse(head: List, k: Int): List = {
    var current = head
    var next: List = Nil
    var prev: List = Nil
    var count = 0

    while (current != Nil && count < k) {
      current match {
        case c: Cons =>
          next = c.tail
          c.tail = prev
          prev = current
          current = next
          count += 1
        case _ => ()
      }
    }

    head match {
      case h: Cons if next != Nil =>
        h.tail = reverse(next, k)
      case _ => ()
    }

    prev
  }

  val data =
    Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8))))))))

  reverse(data, 3)

}
