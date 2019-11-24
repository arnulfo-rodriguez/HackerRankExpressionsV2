package exercises

import exercises.MaxSubsetSum.maxSubsetSumRecStart

import scala.math.max

object MaxSubsetSum extends App {


  def maxSubsetSumRecContinue(arr: List[Int]): Int = {
    arr match {
      case a :: b :: rest =>
        List(
          a,
          a + maxSubsetSumRecContinue(rest),
          maxSubsetSumRecContinue(b :: rest),
          maxSubsetSumRecStart(rest)
        ).max
      case a :: Nil => a
      case Nil => 0
    }
  }

  def maxSubsetSumRecStart(arr: List[Int]): Int = {
    arr match {
      case a :: b :: rest if rest nonEmpty =>
        List(
          a + maxSubsetSumRecContinue(rest),
          maxSubsetSumRecStart(b :: rest),
          maxSubsetSumRecStart(rest)
        ).max
      case _ => 0
    }
  }

  def maxSubsetSum(arr: Array[Int]): Int = {
    maxSubsetSumRecStart(arr.toList)
  }

  println(maxSubsetSum(Array(-1, -2, 3, 10, 5,-10)))

}