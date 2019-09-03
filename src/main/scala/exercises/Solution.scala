package exercises

import java.io.PrintWriter


object Solution {


  // Complete the solve function below.
  def solve(array: Array[Int]): Long = {

    val sortedWithIndex = array.indices.sortWith{(p1,p2) => array(p1) > array(p2)}
    val allSets : Seq[(Int,Int,Int,Int)] = (0 to array.length -2).flatMap{
      i => ((i + 1) until array.length).map{j => (i,j,array(i),array(j))}
    }


    @scala.annotation.tailrec
    def addRec(allSetsContainingPoint: Seq[(Int, Int, Int, Int)], index: Int,acc : Long): Long = allSetsContainingPoint match {
      case (i,j,arr_i,arr_j)::rest if (arr_i * arr_j) <= array(index) => addRec(rest,index,acc + 1)
      case _ => acc
    }

    def solvePoint(index : Int,total : Long,remaining : List[(Int,Int,Int,Int)]) : (Long,List[(Int,Int,Int,Int)]) = {
       val allSetsContainingPoint = remaining.filter{r  =>
         r match {
           case (i,j,_,_) if (i < index) && (j >= index) => true
           case (i,j,_,_) if i == index => true
           case _ => false
         }
       }.sortWith{
         (a,b) => (a._3 * a._4) < (b._3 * b._4)
       }
      (total + addRec(allSetsContainingPoint,index,0l), remaining diff allSetsContainingPoint)
    }

    sortedWithIndex.foldLeft((0l,allSets.toList)){
      (acc,i) => solvePoint(i,acc._1,acc._2)
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
