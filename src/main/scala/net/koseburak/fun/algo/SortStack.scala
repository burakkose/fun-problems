package net.koseburak.fun.algo

object SortStack {
  def sort(stack: List[Int]): List[Int] = {
    def inner(s: List[Int], acc: List[Int] = List.empty): List[Int] =
      s match {
        case h1 :: h2 :: tail =>
          inner(Math.max(h1, h2) :: tail, Math.min(h1, h2) :: acc)
        case _ =>
          acc.reverse ::: s
      }

    (0 until (stack.size - 1))
      .foldLeft(inner(stack)) { case (acc, _) => inner(acc) }
  }

  val stack = List(5, 1, 54, 7, 2)
  sort(stack)
}
