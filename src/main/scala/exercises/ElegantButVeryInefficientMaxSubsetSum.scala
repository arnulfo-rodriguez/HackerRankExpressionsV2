package exercises

import java.io.PrintWriter

object ElegantButVeryInefficientMaxSubsetSum extends App {

  def maxSubsetSum(arr: Array[Int]): Int = {
    val indexes = 1 to arr.length
    val list = allSubsets(indexes,initialSets(indexes.toList))
    list
      .map{l => l.map{i => arr(i - 1)}.sum}
      .max
  }

  def initialSets(arr: List[Int]): Stream[Set[Int]] = {
    arr match {
      case a :: b :: rest => rest.map { x => Set(a, x) }.toStream #::: initialSets(b :: rest)
      case _ => Stream.empty
    }
  }

  def allSubsets(arr : Range,prevGeneration: Stream[Set[Int]]) : Stream[Set[Int]] = {
    val nextGen : Stream[Set[Int]] = arr.toStream.flatMap{
      i => prevGeneration
        .filter{l => !(l.contains(i) || l.contains(i-1) || l.contains(i+1))}
        .map{l => l + i}
    }
   if (nextGen.isEmpty) {
     prevGeneration
   } else {
     prevGeneration #::: nextGen #::: allSubsets(arr,nextGen)
   }
  }

  println(maxSubsetSum(Array(1,2,3,4,5,6,7)))

}
