import java.util.NoSuchElementException

object List {

  trait MyList[+T] {
    def isEmpty: Boolean

    def head: T

    def tail: MyList[T]

    def prepend[U >: T](elem: U): MyList[U] = new Cons(elem, this)
  }

  class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {
    def isEmpty = false
  }

  class Nil[T] extends MyList[T] {
    def isEmpty = true

    def head = throw new NoSuchElementException("Nil.head")

    def tail = throw new NoSuchElementException("Nil.head")
  }

  object MyList {
    def apply[T]() = new Nil

    def apply[T](x: T) = new Cons(x, new Nil)

    def apply[T](x1: T, x2: T) = new Cons(x1, new Cons(x2, new Nil))
  }

  // Testing
  val list_1 = MyList(1, 2)
  val list_2: MyList[Int] = MyList()
  list_1.head
  val list_3 = list_1.prepend(0)
  list_3.head
}
