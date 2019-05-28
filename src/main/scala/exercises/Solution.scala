import java.util.regex.Pattern

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

    var current = 0;
    val isNumber: Pattern = Pattern.compile("\\d");

    def next(): Option[Any] = {
      while (current < input.length && input(current) == SEPARATOR) {
        current += 1
      }
      if (current == input.length) {
        return None
      }
      if (currentIsNumber) {
        val initialIndex = current;
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

    private def currentIsNumber = {
      input(current) >= '0' && input(current) <= '9'
    }

    def pushBack: Unit = {
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
        .map { x =>
          x match {
            case PLUS => Add(expr, parseExpression())
            case MINUS => Sub(expr, parseExpression())
            case _ => tokenizer.pushBack; expr
          }
        }.getOrElse(expr)
    }


    def parseTerm(): Expression = {
      parseFactor().flatMap { expr =>
        tokenizer
          .next()
          .map { x =>
            x match {
              case MUL => Mult(expr, parseTerm())
              case DIV => Div(expr, parseTerm())
              case _ => tokenizer.pushBack; expr
            }
          }.orElse(Some(expr))
      }.get
    }


    def parseFactor(): Option[Expression] = {
      tokenizer
        .next()
        .flatMap {
          case x: Long => Some(Literal(x))
          case OPEN_PAREN => {
            val innerExpr = parseExpression()
            tokenizer
              .next()
              .map {
                case CLOSE_PAREN => innerExpr
                case _ => fail
              }
          };
          case PLUS => {
            parseFactor().map { expr =>
              UnaryPlus(expr)
            }
          }
          case MINUS => {
            parseFactor().map { expr =>
              UnaryMinus(expr)
            }
          }
          case unexpected => tokenizer.pushBack; None
        }
    }


    private def fail: Expression = {
      throw new RuntimeException("Parsing Error!!")
    }
  }


  object Evaluator {
    val p = Add(Pow(Literal(10), Literal(9)), Literal(7))
    val p_minus_two = Add(Pow(Literal(10), Literal(9)), Literal(5))
  }

  class Evaluator() {

    import Evaluator._

    def evaluate(tree: Expression): Long = {
      val transformed = transform(tree)
      println(transformed)
      val simplified = simplify(Modulus(transformed, p))
      println(simplified)
      val result = evaluateRec(simplified)
      result
    }

    private def evaluateRec(expression: Expression): Long = {
      val result = expression match {
        case Literal(x) => x
        case Add(x, y) => evaluateRec(x) + evaluateRec(y)
        case Mult(x, y) => evaluateRec(x) * evaluateRec(y)
        case Sub(x, y) => evaluateRec(x) - evaluateRec(y)
        case UnaryMinus(x) => -1 * evaluateRec(x)
        case Pow(x, y) => scala.math.pow(evaluateRec(x), evaluateRec(y)).toLong
        case Modulus(Pow(x, y), m) => {
          val eval_x = evaluateRec(x)
          val eval_y = evaluateRec(y)
          val eval_m = evaluateRec(m)
          val result = (1l to eval_y - 1).map{_ => eval_x}.foldLeft(eval_x) { (acc, _) => (acc * eval_x) % eval_m }
          println(s"($eval_x ^ $eval_y) % $eval_m = $result")
          result
        }
        case Modulus(x, y) => evaluateRec(x) % evaluateRec(y)
      }
      //println(s"Expression $expression evaluated to $result")
      result
    }

    private def transform(expression: Expression): Expression = {
      expression match {
        case Add(x, y) => Add(transform(x), transform(y))
        case Mult(x, y) => Mult(transform(x), transform(y))
        case Div(x, y) => Mult(transform(x), Pow(transform(y), p_minus_two))
        case Sub(x, y) => Sub(transform(x), transform(y))
        case UnaryMinus(x) => Mult(Literal(-1), transform(x))
        case UnaryPlus(x) => transform(x)
        case x => x
      }
    }

    private def simplify(expression: Expression): Expression = expression match {
      case Modulus(Mult(x, y), z) =>
        val simplified_z = simplify(z)
        Modulus(Mult(simplify(Modulus(simplify(x), simplified_z)), simplify(Modulus(simplify(y), simplified_z))), simplified_z)
      case Pow(x, Add(Pow(a, b), c)) =>
        val simplified_x = simplify(x)
        simplify(Mult(Pow(simplified_x,simplify(Mult(simplify(a),simplify(b)))),simplify(Pow(simplified_x,simplify(c)))))
      case Pow(Pow(a,b),c) => simplify(Pow(a,Mult(b,c)))
      case Add(x, y) => Add(simplify(x), simplify(y))
      case Mult(x, y) => Mult(simplify(x), simplify(y))
      case Sub(x, y) => Sub(simplify(x), simplify(y))
      case Modulus(x, y) => Modulus(simplify(x), simplify(y))
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

