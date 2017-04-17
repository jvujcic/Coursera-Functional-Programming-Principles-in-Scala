import week03._

object nth {
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)

  def nth[T](n: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException()
    else if (n == 0) list.head
    else nth(n - 1, list.tail)
}