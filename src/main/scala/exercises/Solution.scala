<<<<<<< HEAD
package exercises

import java.io.PrintWriter

import scala.annotation.tailrec
import scala.collection.mutable

object Solution {


  def addSorted(i: Int, priorityQueue: Seq[Int]) = {
    @tailrec
    def addSortedRec(newElement: Int, existing: Seq[Int], acc: Seq[Int]): Seq[Int] = existing match {
      case first :: rest if newElement <= first => acc ++ (newElement :: first :: rest)
      case first :: rest => addSortedRec(newElement, rest, acc :+ first)
      case _ => acc :+ newElement
    }

    addSortedRec(i, priorityQueue, Seq())
  }

  // Complete the solve function below.
  def solve(array: Array[Int]): Long = {

    def countWhile(priorityQueue: Seq[Int], max: Int, f: (Int, Int) => Boolean): Long = {
      def spanRec(i: Int, iterator: Seq[Int], acc: Long): Long = iterator match {
        case first :: rest if i == 0 && f(first,max) => spanRec(i+1,rest, acc + 1)
        case first :: rest if i == 0 && f(first,max) => spanRec(i+1,rest, acc + 1)
        case _ => acc
      }

      spanRec(0,priorityQueue, 0l)
    }

    def solveRec(current: Int, priorityQueue: Seq[Int], max: Int, total: Long): Long = current match {
      case 0 =>
        solveRec(1, addSorted(array(current), priorityQueue), array(current), total)
      case i if i < array.length => {
        val iThElement = array(i)
        val newMax = math.max(max, iThElement)
        val matchesCount = countWhile(priorityQueue, newMax, (v, m) => (v.toLong * iThElement.toLong) <= m)
        solveRec(i + 1, addSorted(iThElement, priorityQueue), newMax, total + matchesCount)
      }
      case _ => total
    }

    solveRec(0, Seq(), 0, 0l)
  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(System.out)

    val arrCount = stdin.readLine.trim.toInt

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = solve(arr)

    printWriter.println(result)
=======
import java.io.PrintWriter

object Solution {

  /*
   * Complete the twoStacks function below.
   */
  def twoStacks(x: Int, a: Array[Int], b: Array[Int]): Int = {
    twoStacksRec(x, 0, a, 0, b, scala.collection.mutable.Map[(Int,Int),Int]())
  }

  def twoStacksRec(x: Int, aIndex: Int, a: Array[Int], bIndex: Int, b: Array[Int], results: scala.collection.mutable.Map[(Int,Int),Int]): Int = {
    if (results.contains((aIndex,bIndex))){
      results((aIndex,bIndex))
    } else {
      val withJustA =
        if ((aIndex < a.size) && (a(aIndex) <= x)) {
          1 + twoStacksRec(x - a(aIndex), aIndex + 1, a, bIndex, b, results)
        } else 0
      val withJustB =
        if ((bIndex < b.size) && (b(bIndex) <= x)) {
          1 + twoStacksRec(x - b(bIndex), aIndex, a, bIndex + 1, b, results)
        } else 0
      val result = math.max(withJustA, withJustB)
      results((aIndex,bIndex)) = result
      result
    }
  }
  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(System.out)

    val g = stdin.readLine.trim.toInt

    for (gItr <- 1 to g) {
      val nmx = stdin.readLine.split(" ")

      val n = nmx(0).trim.toInt

      val m = nmx(1).trim.toInt

      val x = nmx(2).trim.toInt

      val a = stdin.readLine.split(" ").map(_.trim.toInt)

      val b = stdin.readLine.split(" ").map(_.trim.toInt)
      val result = twoStacks(x, a, b)

      printWriter.println(result)
    }
>>>>>>> on solution

    printWriter.close()
  }
}
