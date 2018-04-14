package net.koseburak.fun

object CheckLinkedListPalindrome {

  sealed trait List
  case object Nil extends List
  case class Cons(v: Int, tail: List = Nil) extends List

  def isPalindrome(list: List): Boolean = {
    import scala.annotation.tailrec
    @tailrec
    def inner(slow: List, fast: List, stack: List): Boolean =
      (slow, fast) match {
        case (Cons(slowV, slowTail), Cons(_, Cons(_, fastTail))) =>
          inner(slowTail, fastTail, Cons(slowV, stack))
        case (Cons(_, tail), Cons(_, Nil)) =>
          inner(tail, Nil, stack)
        case (_, Nil) =>
          (slow, stack) match {
            case (Cons(slowV, slowTail), Cons(stackV, stackTail)) =>
              if (slowV == stackV) {
                inner(slowTail, fast, stackTail)
              } else {
                false
              }
            case (Nil, Nil) => true
            case _ => false
          }
        case _ => false
      }

    inner(list, list, Nil)
  }

  val list = Cons(5, Cons(4, Cons(3, Cons(4, Cons(5)))))
}
