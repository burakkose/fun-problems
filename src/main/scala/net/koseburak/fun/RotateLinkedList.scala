package net.koseburak.fun

object RotateLinkedList {

  sealed trait List

  case class Cons(data: Int, var tail: List = Nil) extends List

  case object Nil extends List

  def rotate(list: List, k: Int): List = {
    def append(l1: List, l2: List): Unit = {
      l1 match {
        case n@Cons(_, Nil) =>
          n.tail = l2
        case Cons(_, tail) => append(tail, l2)
        case Nil => ()
      }
    }

    def inner(head: List, current: List, counter: Int): List = {
      current match {
        case n@Cons(_, tail) =>
          if (counter < k)
            inner(head, tail, counter + 1)
          else {
            n.tail = Nil
            append(tail, head)
            println(head)
            tail
          }
        case Nil => Nil
      }
    }

    inner(list, list, 1)
  }

  val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6))))))

  rotate(list, 4)

}
