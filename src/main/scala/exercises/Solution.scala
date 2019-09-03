package exercises

import java.io.PrintWriter

object Solution {

  // Complete the solve function below.
  def solve(array: Array[Int]): Long = {

    def accumulate(i: Int, prevCount: Long, prevMax: Int): (Long, Int) = {
      if ((i > 0) && (array(i) == array(i - 1)) && (prevCount > 0)) {
        (
          prevCount - 1,
          prevMax
        )
      } else if ((i > 0) && (array(i) >= array(i - 1)) && (prevCount == 0)) {
        (
          0l,
          math.max(prevMax, array(i))
        )
      } else {
        ((i + 1) until array.length).
          foldLeft((0l, array(i))) {
            (acc, j) =>
              acc match {
                case (count, max) => (
                  count + (if ((array(i).toLong * array(j).toLong) <= math.max(max, array(j))) 1l else 0l),
                  math.max(max, array(j))
                )
              }
          }
      }
    }

    (0 to (array.length - 2))
      .foldLeft((0l, 0l, array(0))) { (acc, i) =>
        val tuple = accumulate(i, acc._2, acc._3)
        (acc._1 + tuple._1, tuple._1, tuple._2)
      }._1
  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(System.out)

    val arrCount = stdin.readLine.trim.toInt

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = solve(arr)

    printWriter.println(result)

    printWriter.close()
  }
}
