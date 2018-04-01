package net.koseburak.fun

object BFSWithPath {
  import scala.annotation.tailrec
  import scala.collection.immutable.Queue
  case class Person(id: Int, friends: Set[Person] = Set.empty)

  def findPath(src: Person, dest: Person): List[Int] = {
    @tailrec
    def backtrace(p: Person,
                  track: Map[Person, Person],
                  acc: List[Int]): List[Int] = {
      if (p == src)
        p.id :: acc
      else
        backtrace(track(p), track, p.id :: acc)
    }
    @tailrec
    def inner(queue: Queue[Person],
              visited: Set[Person],
              track: Map[Person, Person]): List[Int] = {
      queue match {
        case x +: xs =>
          if (visited contains x)
            inner(xs, visited, track)
          else {
            if (x == dest) {
              println(track)
              backtrace(dest, track, List.empty[Int])
            } else
              inner(queue.enqueue(x.friends),
                    visited + x,
                    track ++ x.friends.map(_ -> x).toMap)
          }
        case _ => List.empty[Int]
      }
    }

    inner(Queue[Person](src), Set.empty[Person], Map.empty[Person, Person])
  }

  val person3 = Person(3, Set(Person(2), Person(4)))
  val src = Person(1, Set(person3))

  findPath(Person(4), person3)

}
