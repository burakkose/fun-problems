package net.koseburak.fun

object KMP {

  import scala.annotation.tailrec
  case class KMPUtil private (pattern: String, lps: Array[Int]) {
    def search(text: String): List[Int] = {
      @tailrec
      def inner(i: Int, j: Int, acc: List[Int]): List[Int] = {
        if (i < text.length) {
          val (newI, newJ) =
            if (pattern.charAt(j) == text.charAt(i)) (i + 1, j + 1) else (i, j)
          if (newJ == pattern.length)
            inner(newI, lps(newJ - 1), (newI - newJ) :: acc)
          else if (newI < text.length && pattern.charAt(newJ) != text.charAt(
                     newI)) {
            if (newJ != 0)
              inner(newI, lps(newJ - 1), acc)
            else
              inner(newI + 1, newJ, acc)
          } else inner(newI, newI, acc)
        } else acc
      }

      inner(0, 0, List.empty)
    }
  }

  object KMPUtil {
    def process(pattern: String): KMPUtil = {
      val lps = Array.fill(pattern.length)(0)

      @tailrec
      def inner(i: Int, len: Int): Unit = {
        if (i < pattern.length) {
          if (pattern.charAt(i) == pattern.charAt(len)) {
            lps(i) = len + 1
            inner(i + 1, len + 1)
          } else if (len != 0) {
            inner(i, lps(len - 1))
          } else {
            lps(i) = len
            inner(i + 1, len)
          }
        }
      }

      inner(0, 0)
      KMPUtil(pattern, lps)
    }

  }
  val util = KMPUtil.process("ABABCABAB")
  util.search("ABABDABACDABABCABAB")

}
