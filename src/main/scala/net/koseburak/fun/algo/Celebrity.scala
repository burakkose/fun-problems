package net.koseburak.fun.algo

object Celebrity {
  val arr = Array(
    Array(1, 0),
    Array(1, 0)
  )

  def solve(arr: Array[Array[Int]]): Option[Int] = {
    def knows(a: Int, b: Int): Boolean = arr(a)(b) == 1
    def eliminate(person1: Int, person2: Int): Int =
      if (knows(person1, person2)) person2 else person1
    def isCelebrity(person: Int): Boolean =
      arr.indices.filter(_ != person).exists { p =>
        knows(p, person) && !knows(person, p)
      }
    def inner(stack: List[Int], person1: Int, person2: Int): Option[Int] = {
      if (stack.lengthCompare(1) > 0) {
        val newPerson = stack.head
        if (knows(person1, person2)) {
          inner(stack.tail, newPerson, person2)
        } else {
          inner(stack.tail, person1, newPerson)
        }
      } else {
        val c = if (stack.size == 1) stack.head else person1
        val candidate = eliminate(eliminate(c, person1), person2)
        Some(candidate).filter(isCelebrity)
      }
    }
    val people = arr.indices.toList
    if (people.size > 1) {
      val List(p1, p2) = people.take(2)
      inner(people.drop(2), p1, p2)
    } else None
  }

  solve(arr)
}
