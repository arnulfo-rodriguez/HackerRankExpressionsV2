import java.io._

object Solution {

  import scala.collection.SortedSet


  case class Range(start: Int, end: Int, increase: Int) {
    def this(fromArray: Array[Int]) = this(fromArray(0), fromArray(1), fromArray(2))

    def isBefore(other: Range): Boolean = end < other.start

    def isAfter(other: Range): Boolean = other.end < start

    def splitIntersection(other: Range): Seq[Range] = {

      SortedSet(start, end, other.start, other.end).toSeq match {

        case Seq(p1, p2) if p1 == start && p1 == end => Seq(Range(p1, p1, increase + other.increase), Range(p1 + 1, p2, other.increase))

        case Seq(p1, p2) if p2 == start && p2 == end => Seq(Range(p1, p2 - 1, other.increase), Range(p2, p2, other.increase + increase))

        //Both are the same range
        case Seq(_) | Seq(_, _) => Seq(Range(start, end, other.increase + increase))

        //Intersect in 3 points and create 3 Ranges
        case Seq(p1, p2, p3) if start == p1 && end == p2 && other.start == p2 =>
          Seq(Range(p1, p2 - 1, increase), Range(p2, p2, increase + other.increase), Range(p2 + 1, p3, other.increase))

        case Seq(p1, p2, p3) if other.start == p2 && other.end == p2 =>
          Seq(Range(p1, p2 - 1, increase), Range(p2, p2, increase + other.increase), Range(p2 + 1, p3, increase))

        case Seq(p1, p2, p3) if start == p2 && end == p2 =>
          Seq(Range(p1, p2 - 1, other.increase), Range(p2, p2, increase + other.increase), Range(p2 + 1, p3, other.increase))

        //Sharing borders
        case Seq(p1, p2, p3) if start == p1 && other.start == p1 && end == p2 =>
          Seq(Range(p1, p2, other.increase + increase), Range(p2 + 1, p3, other.increase))

        case Seq(p1, p2, p3) if start == p1 && other.start == p1 && other.end == p2 =>
          Seq(Range(p1, p2, other.increase + increase), Range(p2 + 1, p3, increase))

        case Seq(p1, p2, p3) if start == p1 && end == p3 && other.end == p3 =>
          Seq(Range(p1, p2 - 1, increase), Range(p2, p3, increase + other.increase))

        case Seq(p1, p2, p3) if other.start == p1 && end == p3 && other.end == p3 =>
          Seq(Range(p1, p2 - 1, other.increase), Range(p2, p3, increase + other.increase))

        //Intersect in 4 points
        case Seq(p1, p2, p3, p4) if start == p1 && end == p4 =>
          Seq(Range(p1, p2 - 1, increase), Range(p2, p3, increase + other.increase), Range(p3 + 1, p4, increase))

        case Seq(p1, p2, p3, p4) if start == p2 && end == p3 =>
          Seq(Range(p1, p2 - 1, other.increase), Range(p2, p3, increase + other.increase), Range(p3 + 1, p4, other.increase))

        case Seq(p1, p2, p3, p4) if start == p1 && end == p3 =>
          Seq(Range(p1, p2 - 1, increase), Range(p2, p3, increase + other.increase), Range(p3 + 1, p4, other.increase))

        case Seq(p1, p2, p3, p4) if start == p2 && end == p4 =>
          Seq(Range(p1, p2 - 1, other.increase), Range(p2, p3, increase + other.increase), Range(p3 + 1, p4, increase))

        case _ => throw new RuntimeException("UnExpected")
      }

    }
  }

  def applyQuery(query: Range, currentRanges: Seq[Range]): Seq[Range] = currentRanges match {
    case Nil => Seq(query)
    case head :: _ if query.isBefore(head) => Seq(query) ++ currentRanges
    case head :: tail if query.isAfter(head) => Seq(head) ++ applyQuery(query, tail)
    case head :: tail => head.splitIntersection(query) match {
      case firstN :+ last => firstN ++ applyQuery(last, tail)
      case _ => throw new RuntimeException("It shouldn't be here")
    }
  }

  // Complete the arrayManipulation function below.
  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {
    queries
      .toStream
      .map(q => new Range(q))
 //     .sortBy{r =>  - r.start}
      .foldLeft(Seq[Range]()) {
        (acc, current) => applyQuery(current, acc)
      }
      .maxBy(r => r.increase)
      .increase
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
