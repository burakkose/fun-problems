package net.koseburak.fun.algo

object NextGreaterElement {

  def print(arr: Array[Int]) = {
    import scala.collection.mutable
    val stack = mutable.Stack(arr(0))
    def innerPrint(elem: Int, next: Int): Int = {
      if (elem < next)
        println(s"$elem -> $next")
      if (stack.nonEmpty)
        innerPrint(stack.pop(), next)
      else elem
    }
    arr.indices.tail.foreach { i =>
      val next = arr(i)
      if (stack.nonEmpty) {
        val elem = innerPrint(stack.pop(), next)
        if (elem > next)
          stack.push(elem)
      }
      stack.push(next)
    }

    stack.foreach(i => println(s"$i -> -1"))
    ()
  }

  var arr = Array(11, 13, 21, 3)
  print(arr)
}
