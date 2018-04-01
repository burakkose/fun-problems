package net.koseburak.fun

object BigFileWord {
  import scala.collection.mutable.{HashMap, PriorityQueue}
  def add(inputs: List[String]): Unit = {
    val words = HashMap[String, Int]().withDefaultValue(0)
    val top5 = PriorityQueue[(String, Int)]()(Ordering.by(-_._2))
    inputs.foreach{ word =>
      val a = words(word)
      words.up
    }
  }
}
