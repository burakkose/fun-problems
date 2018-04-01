package net.koseburak.fun

object CycleDetect {
  sealed trait List {
    def size: Int
  }
  case class Cons(value: Int, var tail: List) extends List {
    override def size: Int = 1 + tail.size
  }
  case object Nil extends List {
    override def size: Int = 0
  }

  def detectAndRemove(list: List): Unit = {
    def inner(slow: List, fast: List): Unit = {
      (slow, fast) match {
        case (Cons(_, newSlow @ _), Cons(_, p2 @ Cons(_, newFast @ _))) =>
          if (newSlow eq newFast) {
            newFast.asInstanceOf[Cons].tail = Nil
            println(list)
          } else inner(newSlow, newFast)
        case _ => ()
      }
    }
    inner(list, list)
  }

  detectAndRemove(head)
  println(head.size)
}
