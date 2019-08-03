import java.io._

import scala.annotation.tailrec

object Solution {

  import scala.collection.SortedSet


  trait Tree {
    def add(range: Range): Tree
    def sumIntersections(toFind: Range) : Long

    val furthestRight: Int
  }

  case object emptyTree extends Tree {
    override def add(range: Range): Tree = AugmentedTree.newTree(range)

    override val furthestRight: Int = 0;

    override def sumIntersections(toFind: Range): Long = 0l
  }

  case class AugmentedTree(range: Range, furthestRight: Int, left: Option[AugmentedTree], right: Option[AugmentedTree]) extends Tree {

    def add(otherRange: Range): AugmentedTree = {
      if (otherRange.start == range.start && otherRange.end == range.end) {
        val newRange = Range(range.start, range.end, range.increase + otherRange.increase)
        AugmentedTree(newRange, furthestRight, left, right)
      } else if (otherRange.start <= range.start) {
        left match {
          case Some(l) => AugmentedTree(range, math.max(furthestRight, otherRange.end), Some(l.add(otherRange)), right)
          case None => AugmentedTree(range, math.max(furthestRight, otherRange.end), Some(AugmentedTree.newTree(otherRange)), right)
        }
      } else {
        right match {
          case Some(r) => AugmentedTree(range, math.max(furthestRight, otherRange.end), left, Some(r.add(otherRange)))
          case None => AugmentedTree(range, math.max(furthestRight, otherRange.end), right, Some(AugmentedTree.newTree(otherRange)))
        }
      }
    }

    def sumIntersections(toFind: Range, total : Long): Long = {
      val value = if ((range.start > toFind.end) || (range.end < toFind.start)) 0 else range.increase + total
      (left match {
          case Some(x) => x.sumIntersections(toFind)
          case None => value
        }) +
          (right match {
            case Some(x: AugmentedTree) if x.furthestRight >= toFind.start => x.sumIntersections(toFind,value)
            case _ => value
          })
    }
  }

  object AugmentedTree {
    def newTree(range: Range): AugmentedTree = AugmentedTree(range, range.end, Option.empty, Option.empty)
  }


  case class Range(start: Int, end: Int, increase: Int) {
    def this(fromArray: Array[Int]) = this(fromArray(0), fromArray(1), fromArray(2))

    def isBefore(other: Range): Boolean = end < other.start

    def isAfter(other: Range): Boolean = other.end < start

  }


  // Complete the arrayManipulation function below.
  def arrayManipulation(n: Int, queries: Array[Array[Int]]): Long = {


    val ranges: Array[Range] = queries.map(q => new Range(q))
    val q = ranges
      .foldLeft(emptyTree.asInstanceOf[Tree]) { (acc, current) => acc.add(current)}

    return ranges.map{ r => q.sumIntersections(r) }.max

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
