package net.koseburak.fun.algo

object LongestSubstringWithoutRepeating {
  import scala.annotation.tailrec
  def find(str: String): Int = {
    val length = str.length
    @tailrec
    def inner(index: Int,
              currentLen: Int,
              maxSoFar: Int,
              visited: Map[Char, Int]): Int = {
      if (index < length) {
        val prevIndex = visited.getOrElse(str.charAt(index), Int.MinValue)
        lazy val updatedVisited = visited.updated(str.charAt(index), index)
        if (prevIndex < 0 || index - currentLen > prevIndex) {
          inner(index + 1, currentLen + 1, maxSoFar, updatedVisited)
        } else {
          inner(index + 1, index - prevIndex, Math.max(currentLen, maxSoFar),updatedVisited)
        }
      } else {
        Math.max(currentLen, maxSoFar)
      }
    }
    inner(0, Int.MinValue, Int.MinValue, Map.empty)
  }

  val str = "ABDEFGABEF"

  find(str)

}
