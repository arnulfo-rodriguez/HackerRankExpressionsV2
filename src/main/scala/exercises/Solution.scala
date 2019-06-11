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

  case class Power(base: Expression, exponent: Expression) extends Expression

  case class Symbol(name: String) extends Expression

  //Simplified
  case class Mono(coefficient: Literal, symbol: Symbol, exponent: Literal)


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
      if (currentIsSymbol) {
        val initialIndex = current
        current += 1
        return Some(input.substring(initialIndex, current))
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

    private def currentIsSymbol: Boolean = {
      (input(current) >= 'a' && input(current) <= 'z') || (input(current) >= 'A' && input(current) <= 'Z')
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
    val POW = '^'
    val tokenizer = new Tokenizer(input)

    def parseExpression(): Expression = {
      val expr = parseTerm()
      tokenizer
        .next()
        .map {
          case PLUS => Add(expr, parseExpression())
          case MINUS => Sub(expr, parseExpression())
          case x => tokenizer.pushBack(); expr
        }.getOrElse(expr)
    }


    def parseTerm(): Expression = {
      val expr = parseTermPrime()
      var result = tokenizer
        .next()
        .map {
          case MUL => Mult(expr, parseTerm())
          case DIV => Div(expr, parseTerm())
          case _ => tokenizer.pushBack(); expr
        }.getOrElse(expr)

      var keepTrying = false
      do {
        keepTrying = false
        result = tokenizer
          .next()
          .map {
            case _: String | OPEN_PAREN | _: Long => tokenizer.pushBack(); keepTrying = true; Mult(result, parseTerm())
            case _ => tokenizer.pushBack(); result
          }.getOrElse(result)
      } while (keepTrying)
      result
    }

    def parseTermPrime(): Expression = {
      parseFactor().flatMap { expr =>
        tokenizer
          .next()
          .map {
            case POW => Power(expr, parseTermPrime())
            case _ => tokenizer.pushBack(); expr
          }.orElse(Some(expr))
      }.get
    }

    def parseFactor(): Option[Expression] = {
      tokenizer
        .next()
        .flatMap {
          case x: Long => Some(Literal(x))
          case x: String => Some(Symbol(x))
          case OPEN_PAREN =>
            val innerExpr = parseExpression()
            tokenizer
              .next()
              .map {
                case CLOSE_PAREN => innerExpr
                case x => fail(CLOSE_PAREN, x)
              };
          case PLUS =>
            parseFactor().map { expr =>
              UnaryPlus(expr)
            }
          case MINUS =>
            parseFactor().map { expr =>
              UnaryMinus(expr)
            }
          case x => tokenizer.pushBack(); None
        }
    }


    private def fail(expected: Any, found: Any): Expression = {
      throw new RuntimeException(s"Parsing Error!! expected = '${expected}' but found ''${found}")
    }
  }


  class Evaluator() {

    def evalAdd(add: Add): Expression = add match {
      case Add(Literal(x), Literal(y)) => Literal(x + y)
      case Add(Mult(Symbol(x1), Literal(y1)), Mult(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Mult(evaluate(Add(Literal(y1), Literal(y2))), Symbol(x1)))
      case Add(Mult(Literal(y1), Symbol(x1)), Mult(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Mult(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(Mult(Symbol(x1), Literal(y1)), Mult(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Mult(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(Mult(Literal(y1), Symbol(x1)), Mult(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Mult(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(Div(Symbol(x1), Literal(y1)), Div(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Div(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(Div(Literal(y1), Symbol(x1)), Div(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Div(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(Div(Symbol(x1), Literal(y1)), Div(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Div(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(Div(Literal(y1), Symbol(x1)), Div(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Div(Add(Literal(y1), Literal(y2)), Symbol(x1)))
      case Add(x, y) =>
        val f: (Expression, Expression) => Expression = (e1, e2) => Add(e1, e2)
        reEvaluateIfNeeded(x, y, f)
    }

    def evalSub(sub: Sub): Expression = sub match {
      case Sub(Literal(x), Literal(y)) => Literal(x - y)
      case Sub(Mult(Symbol(x1), Literal(y1)), Mult(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Mult(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Mult(Literal(y1), Symbol(x1)), Mult(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Mult(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Mult(Symbol(x1), Literal(y1)), Mult(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Mult(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Mult(Literal(y1), Symbol(x1)), Mult(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Mult(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Div(Symbol(x1), Literal(y1)), Div(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Div(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Div(Literal(y1), Symbol(x1)), Div(Symbol(x2), Literal(y2))) if x1 == x2 => evaluate(Div(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Div(Symbol(x1), Literal(y1)), Div(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Div(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(Div(Literal(y1), Symbol(x1)), Div(Literal(y2), Symbol(x2))) if x1 == x2 => evaluate(Div(Sub(Literal(y1), Literal(y2)), Symbol(x1)))
      case Sub(x, y) => val f: (Expression, Expression) => Expression = (e1, e2) => Sub(e1, e2)
        reEvaluateIfNeeded(x, y, f)
    }

    def evalMult(mult: Mult): Expression = mult match {
      case Mult(Literal(x), Literal(y)) => Literal(x * y)
      case Mult(Literal(0), _) | Mult(_, Literal(0)) => Literal(0)
      case Mult(Literal(1), x) => evaluate(x)
      case Mult(x, Literal(1)) => evaluate(x)
      case Mult(Literal(x), Mult(Literal(y), z)) => evaluate(Mult(Mult(Literal(x), Literal(y)), z))
      case Mult(x, Add(y, z)) => evaluate(Add(Mult(x, y), Mult(x, z)))
      case Mult(x, Add(y, z)) => evaluate(Add(Mult(x, y), Mult(x, z)))
      case Mult(Add(y, z), x) => evaluate(Add(Mult(x, y), Mult(x, z)))
      case Mult(x, y) => val f: (Expression, Expression) => Expression = (e1, e2) => Mult(e1, e2)
        reEvaluateIfNeeded(x, y, f)
    }

    def evalDiv(div: Div): Expression = div match {
      case Div(Literal(x), Literal(y)) => Literal(x / y)
      case Div(x, Literal(1)) => evaluate(x)
      case Div(x, Literal(1)) => evaluate(x)
      case Div(Add(y, z), x) => evaluate(Add(Div(y, x), Div(z, x)))
      case Div(Mult(x:Literal, y),z:Literal) => evaluate(Mult(Div(x,z),y))
      case Div(x, y) => val f: (Expression, Expression) => Expression = (e1, e2) => Div(e1, e2)
        reEvaluateIfNeeded(x, y, f)
    }

    def evalPower(power: Power) = power match {
      case Power(x, Literal(0)) => Literal(1)
      case Power(x, Literal(1)) => evaluate(x)
      case Power(Literal(x), Literal(y)) => Literal(scala.math.pow(x, y).toLong)
    }

    def evalUnaryMinus(unaryMinus: UnaryMinus): Expression = unaryMinus match {
      case UnaryMinus(Literal(x)) => Literal(x * -1)
      case UnaryMinus(y) => evaluate(Mult(Literal(-1), y))
    }

    def evaluate(tree: Expression): Expression = tree match {
      case add: Add => evalAdd(add)
      case sub: Sub => evalSub(sub)
      case mult: Mult => evalMult(mult)
      case div: Div => evalDiv(div)
      case power: Power => evalPower(power)
      case unaryMinus: UnaryMinus => evalUnaryMinus(unaryMinus)
      case _ => tree
    }

    private def reEvaluateIfNeeded(x: Expression, y: Expression, f: (Expression, Expression) => Expression) = {
      val eval_x = evaluate(x)
      val eval_y = evaluate(y)
      if ((eval_x == x) && (eval_y == y))
        f(x, y)
      else
        evaluate(f(eval_x, eval_y))
    }

    def simplify(tree: Expression): Expression = {
      tree
    }

    private def simplifyRec(expression: Expression): Expression = {
      expression
    }


    def toString(tree: Expression, stringBuilder: StringBuilder): StringBuilder = {

      def toStringBinary(x: Expression, y: Expression, operand: String, parenOptions: ParenOptions) = {
        if (parenOptions == ParenOptions.BOTH || parenOptions == ParenOptions.LEFT) {
          withParen(s => toString(x, s), stringBuilder)
        } else {
          toString(x, stringBuilder)
        }

        stringBuilder.append(operand)

        if (parenOptions == ParenOptions.BOTH || parenOptions == ParenOptions.RIGHT) {
          withParen(s => toString(y, s), stringBuilder)
        } else {
          toString(y, stringBuilder)
        }
      }

      def withParen(printMiddle: StringBuilder => StringBuilder, stringBuilder: StringBuilder) = {
        stringBuilder.append("(")
        printMiddle(stringBuilder)
        stringBuilder.append(")")
      }

      sealed trait ParenOptions

      object ParenOptions {

        case object LEFT extends ParenOptions

        case object RIGHT extends ParenOptions

        case object BOTH extends ParenOptions

        case object NONE extends ParenOptions

      }

      def needsParen(expression: Expression): ParenOptions = expression match {
        case Mult(_: Add | _: Sub, _: Add | _: Sub) => ParenOptions.BOTH
        case Mult(_: Add | _: Sub, _) => ParenOptions.LEFT
        case Mult(_, _: Add | _: Sub) => ParenOptions.RIGHT
        case Div(_: Add | _: Sub, _: Add | _: Sub) => ParenOptions.BOTH
        case Div(_: Add | _: Sub, _) => ParenOptions.LEFT
        case Div(_, _: Add | _: Sub) => ParenOptions.RIGHT
        case Power(_: Symbol | _: Literal, _: Symbol | _: Literal) => ParenOptions.NONE
        case Power(_, _: Symbol | _: Literal) => ParenOptions.LEFT
        case Power(_: Symbol | _: Literal, _) => ParenOptions.RIGHT
        case Power(_, _) => ParenOptions.BOTH
        case Sub(_: Add | _: Sub, _: Add | _: Sub) => ParenOptions.BOTH
        case Sub(_: Add | _: Sub, _) => ParenOptions.LEFT
        case Sub(_, _: Add | _: Sub) => ParenOptions.RIGHT
        case _ => ParenOptions.NONE
      }

      tree match {
        case Literal(x) => stringBuilder.append(x)
        case Symbol(x) => stringBuilder.append(x)
        case a@Add(x, y) => toStringBinary(x, y, " + ", needsParen(a))
        case s@Sub(x, y) => toStringBinary(x, y, " - ", needsParen(s))
        case d@Div(x, y) => toStringBinary(x, y, " / ", needsParen(d))
        case m@Mult(x, y) => toStringBinary(x, y, "", needsParen(m))
        case UnaryPlus(x) => toString(x, stringBuilder)
        case u@UnaryMinus(_: Add | _: Sub) =>
          stringBuilder.append(" -");
          withParen(s => toString(u.innerExpression, s), stringBuilder)
        case u@UnaryMinus(x) => stringBuilder.append("-"); toString(x, stringBuilder)
        case p@Power(x, y) => toStringBinary(x, y, "ˆ", needsParen(p))
      }
    }
  }


  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution*/
    val total = scala.io.StdIn.readLine().trim.toInt

    1 to total foreach { _ =>
      val l = scala.io.StdIn.readLine()
      val tree = new Parser(l).parseExpression()
      val evaluator = new Evaluator()
      println(evaluator.toString(evaluator.evaluate(tree), new StringBuilder).toString())
    }
  }
}

/*
   6
   10x + 2x - (3x + 6)/3
   18*(2x+2) - 5
   ((9x + 81)/3 + 27)/3  - 2x
   18x + (12x + 10)*(2x+4)/2 - 5x
   (2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x
   (2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x

  */
