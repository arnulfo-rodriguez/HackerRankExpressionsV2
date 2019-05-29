import java.util.regex.Pattern

import scala.annotation.tailrec

//Expression ::= Term [+-] Expression
//  | Term
//
//Term       ::= Factor [*/] Term
//| Factor
//
//Factor     ::= Number
//| [+-] Factor
//  | '(' Expression ')'
//


object Solution {


  sealed abstract class Expression

  case class Literal(x: Long) extends Expression

  case class Add(x: Expression, y: Expression) extends Expression

  case class Mult(x: Expression, y: Expression) extends Expression

  case class Div(x: Expression, y: Expression) extends Expression

  case class Sub(x: Expression, y: Expression) extends Expression

  case class UnaryPlus(innerExpression: Expression) extends Expression

  case class UnaryMinus(innerExpression: Expression) extends Expression


  class Tokenizer(input: String) {
    val SEPARATOR = ' '

    var current = 0
    val isNumber: Pattern = Pattern.compile("\\d");

    def next(): Option[Any] = {
      while (current < input.length && input(current) == SEPARATOR) {
        current += 1
      }
      if (current == input.length) {
        return None
      }
      if (currentIsNumber) {
        val initialIndex = current
        while (current < input.length && currentIsNumber) {
          current += 1
        }
        return Some(input.substring(initialIndex, current).toLong)
      }
      if (current < input.length) {
        current += 1
        Some(input(current - 1))
      } else {
        None
      }
    }

    private def currentIsNumber: Boolean = {
      input(current) >= '0' && input(current) <= '9'
    }

    def pushBack(): Unit = {
      if (current > 0) {
        current -= 1
      } else {
        throw new RuntimeException("Tokenizer Error")
      }
    }
  }


  class Parser(input: String) {
    val OPEN_PAREN = '('
    val CLOSE_PAREN = ')'
    val PLUS = '+'
    val MUL = '*'
    val DIV = '/'
    val MINUS = '-'

    val tokenizer = new Tokenizer(input)

    def parseExpression(): Expression = {
      val expr = parseTerm()
      tokenizer
        .next()
        .map {
          case PLUS => Add(expr, parseExpression())
          case MINUS => Sub(expr, parseExpression())
          case _ => tokenizer.pushBack(); expr
        }.getOrElse(expr)
    }


    def parseTerm(): Expression = {
      parseFactor().flatMap { expr =>
        tokenizer
          .next()
          .map {
            case MUL => Mult(expr, parseTerm())
            case DIV => Div(expr, parseTerm())
            case _ => tokenizer.pushBack(); expr
          }.orElse(Some(expr))
      }.get
    }


    def parseFactor(): Option[Expression] = {
      tokenizer
        .next()
        .flatMap {
          case x: Long => Some(Literal(x))
          case OPEN_PAREN =>
            val innerExpr = parseExpression()
            tokenizer
              .next()
              .map {
                case CLOSE_PAREN => innerExpr
                case _ => fail
              };
          case PLUS =>
            parseFactor().map { expr =>
              UnaryPlus(expr)
            }
          case MINUS =>
            parseFactor().map { expr =>
              UnaryMinus(expr)
            }
          case _ => tokenizer.pushBack(); None
        }
    }


    private def fail: Expression = {
      throw new RuntimeException("Parsing Error!!")
    }
  }


  object Evaluator {
    val p = 1000000007l
    val p_minus_two = 1000000005l
  }

  class Evaluator() {

    import Evaluator._

    def evaluate(tree: Expression): Long = {
      val result = evaluateRec(tree)
      if (result < 0) p + result else result
    }

    private def evaluateRec(expression: Expression): Long = {
      val result = expression match {
        case Literal(x) => x
        case Add(x, y) => (evaluateRec(x) + evaluateRec(y)) % p
        case Mult(x, y) => (evaluateRec(x) * evaluateRec(y)) % p
        case Sub(x, y) => (evaluateRec(x) - evaluateRec(y)) % p
        case UnaryMinus(x) => (-1 * evaluateRec(x)) % p
        case Div(x,y) => (evaluateRec(x) * fastExponentiation(evaluateRec(y),p_minus_two,p)) % p
      }
      //println(s"Expression $expression evaluated to $result")
      result
    }


    private def fastExponentiation(a: Long, b: Long, m: Long): Long = b match {
      case 0 => 1
      case even if even % 2 == 0 =>
        val result = fastExponentiation(a, b / 2, m) % m
        (result * result) % m
      case _ =>
        val result = fastExponentiation(a, (b - 1) / 2, m) % m
        (((result * result) % m) * a) % m
    }


  }


  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution*/
    val l = scala.io.StdIn.readLine()

    val tree = new Parser(l).parseExpression()
    //println(tree)
    println(new Evaluator().evaluate(tree))
  }
}

