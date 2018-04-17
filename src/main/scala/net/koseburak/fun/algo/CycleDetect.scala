package net.koseburak.fun.algo

object CycleDetect extends App {
  sealed trait List {
    def size: Int
  }
  case class Cons(value: Int, var tail: List = Nil) extends List {
    override def size: Int = 1 + tail.size
  }
  case object Nil extends List {
    override def size: Int = 0
  }

  def detectAndRemove(list: List): Unit = {
    import scala.annotation.tailrec
    @tailrec
    def destory(slow: List, fast: List): Unit =
      (slow, fast) match {
        case (Cons(m1, slowNext), fast @ Cons(m2, fastNext)) =>
          println(s"destroy $m1 and $m2")
          if (slowNext ne fastNext)
            destory(slowNext, fastNext)
          else fast.tail = Nil
        case _ => ()
      }
    @tailrec
    def inner(slow: List, fast: List): Unit =
      (slow, fast) match {
        case (Cons(_, newSlow: Cons), Cons(_, _ @Cons(_, newFast: Cons))) =>
          println(s"cheching ${newSlow.value} and ${newFast.value}")
          if (newSlow eq newFast) {
            destory(list, newFast)
          } else inner(newSlow, newFast)
        case _ => ()
      }
    inner(list, list)
  }

  val temp: Cons = Cons(15, Cons(4, Cons(10)))
  temp.tail.asInstanceOf[Cons].tail.asInstanceOf[Cons].tail = temp
  val head = Cons(50, Cons(20, temp))

  detectAndRemove(head)
  println(head)
}
