import java.io.{BufferedWriter, PrintWriter}


object TwoStacksSolution {

  import scala.annotation.tailrec
  import scala.collection.immutable.Stream

  class MyStack(private val values: Array[Int], private val index: Int) {
    def peek: Int = values(index)

    def pop: MyStack = new MyStack(values, index + 1)

    def isEmpty: Boolean = index == values.length

    def pushBack: MyStack = new MyStack(values, index - 1)

    def atTop: Boolean = index == 0

    def prevTop: Int = values(index - 1)

    override def hashCode(): Int = index
  }

  case class Turn(a: MyStack, b: MyStack, total: Int, numMoves: Int)

  /*
   * Complete the twoStacks function below.
   */
  def twoStacks(x: Int, a: Array[Int], b: Array[Int]): Int = {
    def solutionFromSingleStack(myStack: MyStack, otherStack: MyStack, remaining: Int, index: Int): Option[Turn] = {
      if ((myStack.isEmpty && remaining <= x && !myStack.atTop) ||
        myStack.peek > remaining) {
        Some(Turn(myStack, otherStack, x - remaining, index))
      } else if (myStack.peek <= remaining) {
        solutionFromSingleStack(myStack.pop, otherStack, remaining - myStack.peek, index + 1)
      } else {
        None
      }
    }

    def moreTurns(currentTurn: Turn): Stream[Turn] =
      Stream.cons(
        currentTurn,
        nextTurn(currentTurn)
          .map(t => moreTurns(t))
          .getOrElse(Stream.empty))


    def nextTurn(turn: Turn): Option[Turn] = turn match {
      case Turn(s1, s2, total, numMoves) if !s2.isEmpty && (total + s2.peek) <= x => Some(Turn(s1, s2.pop, total + s2.peek, numMoves + 1))
      case Turn(s1, s2, total, numMoves) if !s1.atTop => nextViableTurn(s1,s2,total,numMoves)
      case _ => None
    }

    @tailrec
    def nextViableTurn(newA : MyStack, newB : MyStack,newTotal : Int,newNumMoves : Int ) : Option[Turn] = {
      if (newB.isEmpty) {
        None
      } else if ((newTotal + newB.peek) <= x) {
        Some(Turn(newA,newB.pop,newTotal + newB.peek,newNumMoves + 1))
      } else if (!newA.atTop) {
        nextViableTurn(newA.pushBack,newB,newTotal - newA.prevTop, newNumMoves - 1)
      } else {
        None
      }
    }


    val stackA = new MyStack(a, 0)
    val stackB = new MyStack(b, 0)

    solutionFromSingleStack(stackA, stackB, x, 0)
      .map(t => moreTurns(t).maxBy(t => t.numMoves))
      .orElse(solutionFromSingleStack(stackB, stackA, x, 0))
      .get
      .numMoves

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

    printWriter.close()
  }
}
