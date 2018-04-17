package net.koseburak.fun.algo

object NextGreaterWithSameSet {

  def findNext(number: String): Option[String] = {
    import scala.annotation.tailrec
    val len = number.length
    @tailrec
    def crossPoint(i: Int, prev: Char): Option[Int] = {
      if (i >= 0) {
        val current = number.charAt(i)
        if (current < prev) Some(i)
        else crossPoint(i - 1, current)
      } else None
    }
    crossPoint(len - 2, number.charAt(len - 1)).map { i=>
      val (f, s) = number.splitAt(i)
      f + s.last + (s.init.tail + s.head).sorted
    }
  }

  val number = "4321"
  println(findNext(number).getOrElse("Not possible"))
}
