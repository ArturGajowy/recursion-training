package training.recursion.ex01

// -------------------- the DSL --------------------
sealed trait Expr

case class IntValue(v: Int) extends Expr

case class DecValue(v: Double) extends Expr

case class Sum(a: Expr, b: Expr) extends Expr

case class Multiply(a: Expr, b: Expr) extends Expr

case class Square(a: Expr) extends Expr

case class Divide(a: Expr, b: Expr) extends Expr

// -------------------------------------------------

object Ex01_ManualRecursion extends App {

  // it's not even tailrec :(
  def eval(e: Expr): Double =
    e match {
      case IntValue(v)      => v.toDouble
      case DecValue(v)      => v
      case Sum(e1, e2)      => eval(e1) + eval(e2) // would love to have case Sum(e1: Double, e2: Double) => e1 + e2
      case Multiply(e1, e2) => eval(e1) * eval(e2)
      case Divide(e1, e2)   => eval(e1) / eval(e2)
      case Square(e)        => Math.pow(eval(e), 2)
    }

  def prettyPrint(e: Expr): String = e match {
    case IntValue(v)      => v.toString
    case DecValue(v)      => v.toString
    case Sum(e1, e2)      => s"(${prettyPrint(e1)} + ${prettyPrint(e2)})"
    case Multiply(e1, e2) => s"(${prettyPrint(e1)} * ${prettyPrint(e2)})"
    case Divide(e1, e2)   => s"(${prettyPrint(e1)} / ${prettyPrint(e2)})"
    case Square(e)        => s"${prettyPrint(e)}²"
  }

  def optimize(e: Expr): Expr = e match {
    case IntValue(v)                  => e
    case DecValue(v)                  => e
    case Sum(e1, e2)                  => Sum(optimize(e1), optimize(e2))
    case Multiply(e1, e2) if e1 == e2 => Square(optimize(e1))
    case Multiply(e1, e2)             => Multiply(optimize(e1), optimize(e2))
    case Divide(e1, e2)               => Divide(optimize(e1), optimize(e2))
    case Square(e1)                   => Square(optimize(e1))
  }

  val expr1 = Divide(Sum(Square(IntValue(3)), Multiply(IntValue(5), DecValue(-2))), IntValue(0))

  val expr2 = Sum(
    Multiply(IntValue(8), IntValue(8)),
    Multiply(IntValue(4),
      Multiply(Square(IntValue(1)), IntValue(4))
    )
  )

  println(s"Evaluated $expr1: ${eval(expr1)}")
  println(s"Pretty-printed $expr1: ${prettyPrint(expr1)}")
  println(s"Optimized $expr2: ${optimize(expr2)}")

}
