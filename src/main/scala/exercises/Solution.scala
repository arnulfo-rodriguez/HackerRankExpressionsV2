import java.io.PrintWriter


object Solution {

  def max(ranges: Seq[Seq[Range]]): Long = {
    ranges.indices.foldLeft((0l, ranges)) {
      (acc, i) =>
        acc match {
          case (m, f :: s :: rest) =>
            (math.max(m, f.map { r => r.value }.sum), (f.filter(r => r.end > (i+1)) ++ s) :: rest)
          case (m, f :: Nil) =>
            (math.max(m, f.map { r => r.value }.sum), Nil)
        }
    }._1
  }

  case class Range(start: Int, end: Int, value: Long)


  // Complete the arrayManipulation function below.
  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {

    def build(i: Int, acc: (Seq[Range], Seq[Range])): (Seq[Range], Seq[Range]) = acc match {
      case (s1, f :: rest) if (i + 1) == f.start => build(i, (s1 :+ f, rest))
      case (_, _) => acc
    }

    val ranges = queries
      .toStream
      .map { arr => Range(arr(0), arr(1), arr(2)) }

    val byStart: Seq[Range] = ranges
      .sortWith { (r1, r2) => r1.start < r2.start }
      .toList


    val t = (0 until n)
      .foldLeft((Seq[Seq[Range]](), byStart)) {
        (acc, i) => {
          val result = build(i, (Seq[Range](), acc._2))
          (acc._1 :+ result._1, result._2)
        }
      }

    max(t._1)
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
