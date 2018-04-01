package net.koseburak.fun

object FirstNonRepeatingChar {
  import scala.annotation.tailrec
  import scala.collection.immutable.ListSet
  def streamIt(stream: String): String = {
    def getOrIgnore(listSet: ListSet[Char]) =
      listSet.headOption.getOrElse("")

    @tailrec
    def inner(s: String, listSet: ListSet[Char], acc: String): String = {
      if (s == "") acc
      else {
        val current = s.charAt(0)
        if (listSet.contains(current)) {
          val newSet = listSet - current
          inner(s.substring(1), newSet, acc + getOrIgnore(newSet))
        } else {
          inner(s.substring(1), listSet + current, acc + getOrIgnore(listSet))
        }
      }
    }
    inner(stream, ListSet[Char](), "")
  }

  val stream = "geeksforgeeksandgeeksquizfor"

  streamIt(stream)
}
