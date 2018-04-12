// Arithmetic Expressions
// EXPRESSION ::= NUMERAL | ( EXPRESSION OPERATOR EXPRESSION )
// OPERATOR ::= + | -
// NUMERAL is a sequence of digits from the set, {0, 1, 2, ..., 9}
import scala.util.parsing.combinator.JavaTokenParsers

class arithmetic extends  JavaTokenParsers {
}

trait ArithOpTree {
  // Operator Tree (Abstract Syntax Tree)
  sealed abstract class Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class Number(s: String) extends Etree
}
   
object Arithmetic extends JavaTokenParsers with ArithOpTree{
  // Parser
  def parse(source: String): Etree =
    parseAll(expr, source) match {
      case Success(optree,_) => optree
      case _ => throw new Exception("Parse error!")
    }
  def expr: Parser[Etree] = wholeNumber ^^ (Number(_)) |
                            "("~>expr~op~expr<~")" ^^
                            { case e1~"+"~e2 => Add(e1,e2)
                              case e1~"-"~e2 => Sub(e1,e2) }
  def op: Parser[String] = "+" | "-"
  // Interpreter
  def eval(t: Etree): Int = t match {
    case Number(s) => s.toInt
    case Add(e1,e2) => eval(e1) + eval(e2)
    case Sub(e1,e2) => eval(e1) - eval(e2)
  }
  // Controller
  def main(args: Array[String]): Unit = {
    try {
      val source = args(0)
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      println("result : " + eval(optree))
    }
    catch { case e: Exception => println(e)}
  }
}