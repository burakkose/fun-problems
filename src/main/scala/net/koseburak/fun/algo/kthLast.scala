package net.koseburak.fun.algo

import scala.annotation.tailrec

object kthLast {
  sealed trait List
  case class Cons(data: Int, tail: List = Nil) extends List
  case object Nil extends List

  def get(l: List, k: Int): List = {
    @tailrec
    def inner(p1: List, p2: List, c: Int): List = {
      (p1, p2) match {
        case (Cons(_, p1tail), Cons(_, p2tail)) =>
          if (c < k)
            inner(p1tail, p2, c + 1)
          else
            inner(p1tail, p2tail, c + 1)
        case _ => p2
      }
    }
    inner(l, l, 0)
  }
}
