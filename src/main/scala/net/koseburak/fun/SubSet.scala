package net.koseburak.fun

object SubSet {
  def xcombination[T](list: List[T], n: Int): List[List[T]] = {
    if (n > list.size) List.empty[List[T]]
    else
      list match {
        case _ :: _ if n == 1 => list.map(List(_))
        case h :: tail =>
          xcombination(tail, n - 1).map(h :: _) ::: xcombination(tail, n)
        case _ => List.empty[List[T]]
      }
  }

  def xsubset[T](l: List[T]): List[List[T]] = {
    (2 to l.size).foldLeft(xcombination(l, 1))((acc, index) =>
      xcombination(l, index) ::: acc)
  }

  xsubset(List("a", "b", "c", "d"))
}
