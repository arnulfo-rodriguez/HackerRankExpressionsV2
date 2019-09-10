package exercises

import java.io.PrintWriter

import scala.annotation.tailrec
import scala.collection.mutable

object Solution {



  def addSorted(i: Int,priorityQueue: Seq[Int]) = {
    @tailrec
    def addSortedRec(newElement: Int, existing: Seq[Int], acc: Seq[Int]): Seq[Int] = existing match {
      case first :: rest if newElement <= first => acc ++ (newElement :: first :: rest)
      case first :: rest => addSortedRec(newElement, rest, acc :+ first)
      case _ => acc :+ newElement
    }
    addSortedRec(i,priorityQueue,Seq())
  }

  // Complete the solve function below.
  def solve(array: Array[Int]): Long = {

    def countWhile(priorityQueue: Seq[Int],  f:  Int => Boolean) : Long = {
      def spanRec(iterator: Seq[Int], acc: Long): Long = iterator match {
        case first::rest if f(first) => spanRec(rest,acc + 1)
        case _ => acc
      }
      spanRec(priorityQueue, 0l)
    }

    def solveRec(current: Int, priorityQueue: Seq[Int], max : Int, total : Long): Long = current match {
      case 0 =>
        solveRec(1,addSorted(array(current),priorityQueue),array(current),total)
      case i if i < array.length => {
        val iThElement = array(i)
        val newMax = math.max(max, iThElement)
        val matchesCount = countWhile(priorityQueue,v => (v.toLong * iThElement.toLong) <= newMax)
        solveRec(i + 1,addSorted(iThElement,priorityQueue),newMax, total + matchesCount)
      }
      case _ => total
    }
    solveRec(0,Seq(),0,0l)
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
