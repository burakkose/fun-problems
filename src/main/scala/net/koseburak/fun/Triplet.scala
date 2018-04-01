package net.koseburak.fun

object Triplet {
  def isTriplet(arr: Array[Int]): Boolean = {
    val newArr = arr.map(num => num * num).sorted
    for{
      i <- newArr.length - 1 to 2 by -1
    } yield {
      var l = 0
      var r = i - 1
      while(l < r) {
        if(arr(l) + arr(r) == arr(i)) {
          return true
        } else if(arr(l) + arr(r) < arr(i)) {
          l += 1
        } else r -= 1
      }
    }
    false
  }
}
