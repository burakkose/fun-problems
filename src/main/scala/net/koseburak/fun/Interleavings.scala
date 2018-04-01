package net.koseburak.fun

object Interleavings {
  def print(s1: String, s2: String): List[String] = {
    def inner(is1: Int, is2: Int, sol: String): List[String] = {
      if (is1 >= s1.length && is2 >= s2.length) {
        return List(sol)
      }
      val a = if (is1 < s1.length) {
        inner(is1 + 1, is2, sol + s1.charAt(is1))
      } else List.empty[String]

      val b = if (is2 < s2.length) {
        inner(is1, is2 + 1, sol + s2.charAt(is2))
      } else List.empty[String]

      a ::: b
    }
    inner(0, 0, "")
  }

  print("AB", "CD")
}
