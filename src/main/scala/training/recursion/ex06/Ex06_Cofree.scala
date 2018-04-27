package training.recursion.ex06

import matryoshka.data._
import matryoshka._
import matryoshka.implicits._
import matryoshka.patterns._
import scalaz._

// -------------------- the DSL --------------------
sealed trait Expr[A]

case class IntValue[A](v: Int)    extends Expr[A]
case class DecValue[A](v: Double) extends Expr[A]
case class Sum[A](a: A, b: A)     extends Expr[A]
case class Square[A](a: A)        extends Expr[A]

sealed trait ExprType
case object IntExpr extends ExprType
case object DecExpr extends ExprType
// -------------------------------------------------

object Ex06_Cofree extends App with Ex06_Traverse {

  // ---------- labelling expressions with Cofree

  val inferType: Algebra[Expr, Cofree[Expr, ExprType]] = {
    case x @ IntValue(v)    => Cofree(IntExpr, x) // note that type order here is switched
    case x @ DecValue(v)    => Cofree(DecExpr, x)
    case s @ Sum(a, b)  => (a.head, b.head) match {
      case (IntExpr, IntExpr) => Cofree(IntExpr, s)
      case _ => Cofree(DecExpr, s)
    }
    case sq @ Square(a) => Cofree(a.head, sq)
  }

  def evalToString(exp: Expr[String]): String = exp match {
    case IntValue(v) => v.toString
    case DecValue(v) => v.toString
    case Sum(d1, d2) => s"($d1 + $d2)"
    case Square(d)   => s"($d²)"
  }

  val expr1: Fix[Expr] =
    sum(
      square(int(3)),
      sum(int(5), int(-20))
    )

  val expr2: Fix[Expr] =
    sum(
      square(int(3)),
      sum(int(5), dec(-20.2))
    )

  val typedExpr1:
    Cofree[Expr, ExprType] = expr1.cata(inferType)
  val typedExpr2: Cofree[Expr, ExprType] = expr2.cata(inferType)
//
  println(typedExpr1.head)
  println(typedExpr2.head)

  val toTypedStr: Algebra[EnvT[ExprType, Expr, ?], String] = {
    case EnvT((exprType, IntValue(v))) => s"($v): $exprType"
    case _                             => ???
  }

  println(typedExpr1.cata(toTypedStr))
  println(typedExpr2.cata(toTypedStr))
}
