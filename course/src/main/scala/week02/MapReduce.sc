object MapReduce {
  def mapReduce(f: Int => Int, g: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else g(f(a), mapReduce(f, g, zero)(a + 1, b))

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def factorial(n: Int) = product(x => x)(1, n)

  // Testing
  factorial(4)
  mapReduce(x => x * x, (x, y) => x + y, 0)(1, 3)
}
