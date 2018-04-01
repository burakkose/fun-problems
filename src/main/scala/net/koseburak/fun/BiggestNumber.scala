package net.koseburak.fun

object BiggestNumber {

  def findMax(arr: Array[String]): String = {
    arr.sortWith { case (x, y) =>
      x + y > y + x
    }.mkString
  }

  val arr = Array("54", "546", "548", "60")

  findMax(arr)

}
