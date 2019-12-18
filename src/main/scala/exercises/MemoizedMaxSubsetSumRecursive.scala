package exercises

object MaxSubsetSumRecursive extends App {

  class AllNonSuccessiveSets(arr: List[Int]) {

    def allSetsRecStart(arr: List[Int]): Stream[Set[Int]] = {
      arr match {
        case a :: b :: rest if rest nonEmpty =>
          allSetsRecContinue(rest).map(s => s + a) #:::
            allSetsRecStart(b :: rest) #:::
            allSetsRecStart(rest)
        case _ => Stream.empty
      }
    }

    def allSetsRecContinue(arr: List[Int]): Stream[Set[Int]] = {
      arr match {
        case a :: b :: rest =>
          Set(a) #:: allSetsRecContinue(rest).map(s => s + a) #:::
            allSetsRecContinue(b :: rest) #:::
            allSetsRecStart(rest)
        case a :: Nil => Stream(Set(a))
        case Nil => Stream.empty
      }
    }

    def allSets(): Stream[Set[Int]] = {
      allSetsRecStart(arr)
    }
  }

  def maxSubsetSum(arr: Array[Int]): Int = {
    val intsToInt = new AllNonSuccessiveSets(arr.toList)
      .allSets()
      .groupBy(x => x)
      .map(s => (s._1, s._2.size))
    intsToInt
      .foreach(s => println(s))
    0
  }

  println(maxSubsetSum(Array(0, 1, 2, 3, 4, 5, 6, 7, 8)))

}