package net.koseburak.fun.algo

object EditDistance {
  //https://www.geeksforgeeks.org/dynamic-programming-set-5-edit-distance/


  def solve(s1: String, s2: String): Int = {
    val dp = Array.ofDim[Int](s1.length + 1, s2.length + 1)
    for {
      i <- 0 to s1.length
      j <- 0 to s2.length
    } {
      if(i == 0)
        dp(i)(j) = j // insert all
      else if(j == 0) //remove all
        dp(i)(j) = i
      else if(s1.charAt(i-1) == s2.charAt(j-1))
        dp(i)(j) = dp(i-1)(j-1)
      else
        dp(i)(j) = 1 + List(dp(i)(j-1), dp(i-1)(j), dp(i-1)(j-1)).min
    }
    dp(s1.length)(s2.length)
  }


  val str1 = "sunday"
  val str2 = "saturday"

  solve(str1, str2)

}
