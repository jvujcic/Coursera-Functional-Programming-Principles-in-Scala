object ListManipulation {

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](xs: List[T], n: Int) =
    (xs take n) ::: (xs drop n + 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (y :: ys) :: zs => flatten(y :: ys) ::: flatten(zs)
    case y :: ys => y :: flatten(ys)
  }

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  // Testing
  val list1 = List(1, 2, 3)
  last(list1)
  init(list1)

  val list2 = List(4, 5, 6)
  concat(list1, list2)

  reverse(list1)

  removeAt(list1, 1)

  val list3 = List(List(1, 2), 3, List(4, 5))
  flatten(list3)

  val list4 = List("a", "a", "a", "b", "c", "c", "a")
  pack(list4)

  encode(list4)
}
