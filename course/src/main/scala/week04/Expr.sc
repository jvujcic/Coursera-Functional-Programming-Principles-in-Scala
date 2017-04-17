object Expr {

  trait Expr {
    def eval: Int
  }

  case class Num(n: Int) extends Expr {
    def eval = n
  }

  case class Sum(e1: Expr, e2: Expr) extends Expr {
    def eval = e1.eval + e2.eval
  }

  def eval(e: Expr): Int = e match {
    case Num(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = e match {
    case Num(n) => n.toString
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
  }

  // Testing
  val n = Num(2)
  val m = Num(3)
  val e = Sum(n, m)
  e.eval // using class hierarchy
  eval(e) // using pattern matching
  show(e) // using pattern matching
}
