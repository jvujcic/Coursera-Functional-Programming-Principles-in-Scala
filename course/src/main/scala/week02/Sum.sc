object Sum {

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)

  // Testing
  sum(x => x * x)(1, 2)
}
