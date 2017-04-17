object Rationals {

  class Rational(x: Int, y: Int) {
    require(y != 0, "Denominator must be nonzero !")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    private def g = gcd(x, y)

    def numer = x / g

    def denom = y / g

    def <(that: Rational) = numer * that.denom < that.numer * denom

    def max(that: Rational) = if (this < that) that else this

    def +(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

    def unary_- = new Rational(-numer, denom)

    def -(that: Rational) = this + (-that)

    override def toString = numer + "/" + denom
  }

  // Testing
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x - y - z
  y + y
  x - y
  x max y

  new Rational(2)
}
