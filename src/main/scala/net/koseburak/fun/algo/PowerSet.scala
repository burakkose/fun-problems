package net.koseburak.fun.algo

object PowerSet {

  def powerSet(set: Set[String]): Seq[List[String]] = {
    val setSize = set.size
    val setWithIndex = set.zipWithIndex.toList
    (0 until (1 << setSize)).map { i =>
      setWithIndex.collect{
        case (elem, idx) if (i & (1 << idx)) > 0 => elem
      }
    }
  }

  val set = Set("a", "b", "c", "d")
  powerSet(set).find(_.mkString == "acb")
}
