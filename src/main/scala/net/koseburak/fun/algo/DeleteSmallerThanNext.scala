package net.koseburak.fun.algo

object DeleteSmallerThanNext {
  def remove(l: List[Int], k : Int): List[Int] = {
    l match {
      case h :: tail =>
        val stack = scala.collection.mutable.Stack[Int](h)
        var count = 0
        for(elem <- tail) {
          while(stack.nonEmpty && stack.head < elem && count < k) {
            stack.pop()
            count += 1
          }
          stack.push(elem)
        }
        stack.reverse.toList
      case _ => l
    }
  }
  val list = List(20, 10, 25, 30, 40)
  remove(list, 2)
}
