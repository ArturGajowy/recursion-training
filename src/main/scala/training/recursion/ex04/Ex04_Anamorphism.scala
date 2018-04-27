package training.recursion.ex04

import matryoshka._
import matryoshka.implicits._
import matryoshka.data._

// -------------------- the DSL --------------------
sealed trait Expr[A]

case class IntValue[A](v: Int)     extends Expr[A]
case class Sum[A](a: A, b: A)      extends Expr[A]
case class Multiply[A](a: A, b: A) extends Expr[A]
// -------------------------------------------------

object Ex04_Anamorphism extends App with Ex04_Traverse {

  // Int => Expr[Int] consisting of only 1s and 2s
  val toBinary: Coalgebra[Expr, Int] = (n: Int) =>
    n match {
      case 0 => IntValue[Int](n)
      case 1 => IntValue[Int](n)
//      case 2 => IntValue[Int](n)
      case even if n % 2 == 0 => Multiply(2, n / 2)
      case odd if n % 2 == 1 => Sum(1, n - 1)
  }

  val toText: Algebra[Expr, String] = {
    case IntValue(v)    => v.toString
    case Sum(a, b)      => s"($a + $b)"
    case Multiply(a, b) => s"($a * $b)"
  }

  val expr = 31.ana.apply[Nu[Expr]](toBinary)
  println(expr)
//  println(expr.cata(toBinary))
//  println(31.hylo(toText, toBinary))
}
