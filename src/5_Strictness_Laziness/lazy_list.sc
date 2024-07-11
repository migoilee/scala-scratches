// How can we avoid traversing a list multiple times and creating multiple lists
// e.g. List(1,2,3).map(_ + 1).filter(_ < 4).map(_ * 2)
// .. List(1,2,3)
// .. List(2,3,4)
// .. List(2,3)

object Stream {
  // Cons smart constructor
  def cons[A](head: => A, tail: Stream[A]): Stream[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(hd, _) => Some(hd()) // explicitly force the thunk to extract ħ
  }

  def toList: List[A] = this match {
    case Cons(hd, tl) => List(hd()) ++ tl().toList
    case Empty => List.empty
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(hd, tl) if n > 1 => Stream.cons(hd(), tl().take(n - 1))
    case Cons(hd, _) if n == 1 => Stream.cons(hd(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tl) if n > 1 => tl().drop(n - 1)
    case Cons(_, tl) if n == 1 => tl()
    case _ => this
  }

  def takeWhile(predicate: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if (predicate(hd())) =>
      Stream.cons(hd(), tl().takeWhile(predicate))
    case _ => Stream.empty
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

// Demonstrating how smart constructor cons deals with memoizing arguments so they only get evaluated once
// memoization: storing the results of expensive function calls to pure functions
// .. and returning the cached result when the same inputs occur again
def foo(i: Int): Int = {
  println("evaluating foo")
  i + i
}

// Normal constructor:
val x1 = Cons(() => foo(1), () => Stream.empty)
x1.headOption
x1.headOption // foo is evaluated both times

// Smart constructor:
val x2 = Stream.cons(foo(1), Stream.empty)
x2.headOption
x2.headOption // foo is not evaluated a second time

//tests
Stream(1, 2, 3).take(2).toList
Stream(1, 2, 3, 4, 5).drop(2).toList
Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 != 0).toList