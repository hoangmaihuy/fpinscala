package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { (name, exprSignal) =>
      (name, Signal.apply(eval(exprSignal(), namedExpressions.removed(name))))
    }
  }


  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = {
    expr match {
      case Expr.Literal(x) => x
      case Expr.Ref(name) => eval(getReferenceExpr(name, references), references.removed(name))
      case Expr.Plus(a, b) => eval(a, references) + eval(b, references)
      case Expr.Minus(a, b) => eval(a, references) - eval(b, references)
      case Expr.Times(a, b) => eval(a, references) * eval(b, references)
      case Expr.Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
