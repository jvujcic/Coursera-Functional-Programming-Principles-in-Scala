object Mergesort {

  def mergesort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(mergesort(fst)(ord), mergesort(snd)(ord))
    }
  }

  // Testing
  val nums = List(2, 1, 9, 6, 7, 3, 4)
  mergesort(nums)
  mergesort(nums)(Ordering.Int.reverse)
}
