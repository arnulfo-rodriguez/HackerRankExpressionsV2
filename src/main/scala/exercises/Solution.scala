import java.io.PrintWriter


object Solution {


  def max(n: Int, byStart: Seq[Range]): Long = {

    @scala.annotation.tailrec
    def maxRec(max: Long, iTotal: Long, i: Int, currentSet: scala.collection.immutable.Map[Int, Long], ranges: Seq[Range]): Long = ranges match {
      case current :: rest if current.start == i =>
        maxRec(max,
          iTotal + current.value,
          i,
          currentSet + (current.end -> (currentSet.getOrElse(current.end, 0l) + current.value)),
          rest
        )
      case _ if i <= n => {
        val newMax = math.max(max, iTotal)
        val toRemove = currentSet.getOrElse(i, 0l)
        val newITotal = iTotal - toRemove
        maxRec(newMax, newITotal, i + 1, currentSet, ranges)
      }
      case _ if i > n => {
        max
      }
    }

    maxRec(0l, 0l, 1, scala.collection.immutable.Map(), byStart)
  }



  case class Range(start: Int, end: Int, value: Long)


  // Complete the arrayManipulation function below.
  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {

    val byStart: Seq[Range] = queries
      .toStream
      .map { arr => Range(arr(0), arr(1), arr(2)) }
      .sortWith { (r1, r2) => (r1.start < r2.start) || ((r1.start == r2.start) && (r1.end < r2.end)) }

    max(n, byStart)
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(System.out)

    val nm = stdin.readLine.split(" ")

    val n = nm(0).trim.toInt

    val m = nm(1).trim.toInt

    val queries = Array.ofDim[Int](m, 3)

    for (i <- 0 until m) {
      queries(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val result = arrayManipulation(n, queries)

    printWriter.println(result)

    printWriter.close()
  }
}
