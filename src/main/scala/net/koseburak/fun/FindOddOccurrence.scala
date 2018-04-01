package net.koseburak.fun

object FindOddOccurrence {

  def find(arr: Array[Int]): Option[Int] = {
    arr.reduceLeftOption(_ ^ _).flatMap { res =>
      if(res == 0) None
      else Some(res)
    }
  }

  val arr = Array[Int](2, 3, 5, 4, 5, 2, 4, 3, 5, 2, 4, 4, 2)

  find(arr)
}
