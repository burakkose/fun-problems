package net.koseburak.fun

object SmallestWindow {

}

object Solution1 { // O (n ^ 3)
  val string = "this is a test string"
  val pattern = "tist"

  val patternCharMap = pattern.groupBy(identity).mapValues(_.length)
  val substrings = string.inits.flatMap(_.tails).toList

  import scala.annotation.tailrec

  @tailrec
  def predicate(map: Map[Char, Int], s: String): Boolean =
    if (map.isEmpty) true
    else if (s.isEmpty) map.isEmpty
    else {
      if (map.contains(s.head)) {
        val count = map(s.head)
        if (count == 1)
          predicate(map - s.head, s.tail)
        else if (count > 0)
          predicate(map.updated(s.head, count - 1), s.tail)
        else false
      } else predicate(map, s.tail)
    }

  substrings.filter(predicate(patternCharMap, _)).minBy(_.length)
}
