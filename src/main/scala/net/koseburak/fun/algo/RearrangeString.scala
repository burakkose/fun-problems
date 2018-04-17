package net.koseburak.fun.algo

import scala.collection.mutable

object RearrangeString {
  val a = "aaabc".groupBy(identity).mapValues(_.length).toList

  val pq = mutable.PriorityQueue()(Ordering.by[(Char, Int), Int](_._2)) ++ a
  var result = ""
  var prev: (Char, Int) = ('$', -1)

  while (pq.nonEmpty) {
    val top = pq.dequeue
    result += top._1
    if (prev._2 > 0)
      pq enqueue prev

    prev = (top._1, top._2 - 1)
  }

  if(prev._2 != 0)
    println("failed man")

  println(result)
}
