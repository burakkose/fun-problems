package net.koseburak.fun

object LengthEncoding {

  def encode(str: String): String = {
    def append(s: String, count: Int, char: Char) = {
      s + s"$char$count"
    }
    def inner(acc: String, count: Int, i: Int): String = {
      if (i >= str.length)
        if (count > 0)
          append(acc, count, str.charAt(i-1))
        else acc
      else if (str.charAt(i) == str.charAt(i - 1)) {
        inner(acc, count + 1, i + 1)
      } else inner(append(acc, count, str.charAt(i - 1)), 1, i + 1)
    }
    if (str.nonEmpty)
      inner("", 1, 1)
    else str
  }

  val str = "a"
  encode(str)

}
