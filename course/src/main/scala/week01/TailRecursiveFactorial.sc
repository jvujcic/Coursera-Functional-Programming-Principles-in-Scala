object TailRecursiveFactorial {

  def factorial(n: Int) = {
    def tailRecFact(m: Int, step: Int): Int =
      if (step == 0) return m
      else return tailRecFact(m * step, step - 1)

    tailRecFact(n, n - 1)
  }

  // Testing
  factorial(3)
  factorial(4)
  factorial(5)
}
