package net.koseburak.fun

// https://www.geeksforgeeks.org/design-and-implement-special-stack-data-structure/
object SpecialStack {

  class SpecialStack {
    private var stack = List.empty[Int] // because mutable stack is deprecated and suggestion is to use List with var
    private var min = List.empty[Int]

    def isEmpty: Boolean = stack.isEmpty

    def push(x: Int): Unit =
      if (isEmpty) {
        stack = x :: stack
        min = x :: min
      } else {
        stack = x :: stack
        val minTop = min.head
        if (x <= minTop)
          min = x :: stack
      }

    def pop: Int =
      (stack, min) match {
        case (sH :: stackTail, minH :: minTail) =>
          stack = stackTail
          if (minH == sH)
            min = minTail
          sH
        case _ => throw new NoSuchElementException
      }

    def getMin: Int = min.head
  }

  val specialStack = new SpecialStack
  specialStack.push(10)
  specialStack.push(20)
  specialStack.push(30)
  println(specialStack.getMin)
  specialStack.push(5)
  println(specialStack.getMin)

}
