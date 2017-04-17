object NewtonSqrt {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double): Double = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs((guess * guess - x) / x) < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  // Testing
  sqrt(2)
  sqrt(1.0e-6)
  sqrt(1.0e50)
}
