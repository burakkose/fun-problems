package net.koseburak.fun.algo

import scala.annotation.tailrec

object Stock {

  def solve(stocks: Array[Int]): List[(Int, Int)] = {
    val n = stocks.size
    @tailrec
    def localMinima(c: Int): Int = {
      if ((c < n - 1) && (stocks(c + 1) <= stocks(c)))
        localMinima(c + 1)
      else c
    }
    @tailrec
    def localMaxima(c: Int): Int = {
      if ((c < n) && (stocks(c) >= stocks(c - 1))) {
        localMaxima(c + 1)
      } else
        c
    }
    @tailrec
    def inner(acc: List[(Int, Int)], idx: Int): List[(Int, Int)] = {
      if (idx >= n - 1)
        acc
      else {
        val localMinimaIdx = localMinima(idx)
        println(localMinimaIdx)
        if (localMinimaIdx == n - 1)
          acc
        else {
          val localMaximaIdx = localMaxima(localMinimaIdx + 1)
          println(localMaximaIdx)
          inner(acc :+ (localMinimaIdx, localMaximaIdx - 1), localMaximaIdx)
        }
      }
    }
    inner(List.empty[(Int, Int)], 0)
  }

  val arr = Array(100, 180, 260, 310, 40, 535, 695)
  solve(arr)
}
