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

  case class Pow(base: Expression, exponent: Expression) extends Expression

  case class Modulus(dividend: Expression, divisor: Expression) extends Expression

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
    val p = Literal(1000000007)
    val p_minus_two = Literal(1000000005)
  }

  class Evaluator() {

    import Evaluator._

    def evaluate(tree: Expression): Long = {
      val transformed = transform(Modulus(tree, p))
      println(transformed)
      val result = evaluateRec(transformed)
      result
    }

    private def evaluateRec(expression: Expression): Long = {
      val result = expression match {
        case Literal(x) => x
        case Add(x, y) => evaluateRec(x) + evaluateRec(y)
        case Mult(x, y) => evaluateRec(x) * evaluateRec(y)
        case Sub(x, y) => evaluateRec(x) - evaluateRec(y)
        case UnaryMinus(x) => -1 * evaluateRec(x)
        case Pow(x, y) =>
          scala.math.pow(evaluateRec(x), evaluateRec(y)).toLong
        case Modulus(Pow(x, y), m) =>
          val eval_x = evaluateRec(x)
          val eval_y = evaluateRec(y)
          val eval_m = evaluateRec(m)
          fastExponentation(eval_x,eval_y,eval_m)
        case Modulus(x, y) =>
          evaluateRec(x) % evaluateRec(y)
      }
      //println(s"Expression $expression evaluated to $result")
      result
    }

    @tailrec
    private def modularExponentiation(x: Long, y: Long, m: Long, acc: Long): Long = y match {
      case 0 => acc
      case _ => modularExponentiation(x, y - 1, m, acc * x % m)
    }

    private def fastExponentation(a: Long, b: Long, m: Long): Long = b match {
      case 0 => 1
      case even if even % 2 == 0 => scala.math.pow(fastExponentation(a, b / 2, m), 2).toLong % m
      case _ => (scala.math.pow(fastExponentation(a, (b - 1) / 2, m), 2).toLong * a) % m
    }


    def isAlreadySimplified(x: Expression, y: Expression): Boolean = (x, y) match {
      case (Modulus(_, _), Modulus(_, _)) => true;
      case _ => false
    }

    private def transform(expression: Expression): Expression = expression match {
      case Modulus(Modulus(a, b), c) if b == c => transform(Modulus(a, b))
      case Modulus(Mult(x,y),m) => transform(Mult(Modulus(x,m),Modulus(y,m)))
      case Div(x, y) => transform(Modulus(Mult(x, Pow(y, p_minus_two)), p))
      case Pow(Pow(a, b), c) => transform(Pow(a, Mult(b, c)))
      case Add(x, y) => Add(transform(x), transform(y))
      case Mult(x, y) => Mult(transform(x), transform(y))
      case Sub(x, y) => Sub(transform(x), transform(y))
      case Modulus(x, y) =>
        val transformed_x = transform(x)
        val transformed_y = transform(y)
        val result = Modulus(transformed_x, transformed_y)
        if (transformed_x == x && transformed_y == y) {
          result
        } else {
          transform(result)
        }
      case x => x

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

