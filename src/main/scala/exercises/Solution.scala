package exercises

import java.io.PrintWriter

object Solution {

  // Complete the solve function below.
  def solve(arr: Array[Int]): Long = {
    def solveRec(start: Int, prevArrayCount: Long, prevMax: Int): (Long, Int) = {
      if (start > 0 && arr(start) <= arr(start - 1) && arr(start) <= prevMax) {
        if (arr(start).toLong * arr(start - 1) <= prevMax) {
          (prevArrayCount - 1, prevMax)
        } else {
          (prevArrayCount, prevMax)
        }
      } else {
        ((start + 1) until arr.length)
          .foldLeft((0l, arr(start))) {
            (acc, j) => {
              acc match {
                case (count, max) => {
                  (count + (if ((arr(start).toLong * arr(j).toLong) <= math.max(arr(j), max)) 1l else 0l),
                    math.max(arr(j), max)
                  )
                }
              }
            }
          }
      }
    }

    (0 to arr.length - 2)
        .map
    solveRec(0,0l,0)

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
