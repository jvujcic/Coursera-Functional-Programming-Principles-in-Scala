object TailRecursiveSum {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a + 1, f(a))
  }

  // Testing
  sum(x => x * x)(1, 3)
}
