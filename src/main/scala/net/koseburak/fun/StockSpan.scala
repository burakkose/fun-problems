package net.koseburak.fun

object StockSpan {

  def solution(arr: Array[Int]): Array[Int] = {
    import scala.collection.mutable
    val solution = Array.fill(arr.length)(0)
    val stack = mutable.Stack(0)
    solution(0) = 1
    for {
      i <- 1 until arr.length
    } yield {
      while(stack.nonEmpty && arr(stack.head) <= arr(i)) {
        stack.pop()
      }
      solution(i) = if(stack.isEmpty) i + 1 else i - stack.head
      stack.push(i)
    }

    solution
  }

  val array = Array(10, 4, 5, 90, 120, 80)

  solution(array)

}
