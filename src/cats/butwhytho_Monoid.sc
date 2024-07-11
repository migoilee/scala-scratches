// https://typelevel.org/cats/typeclasses.html

object Whatdowegottado {

  def sumInts(list: List[Int]): Int = list.foldRight(0)(_ + _)

  def concat(list: List[String]): String = list.foldRight("")(_ ++ _)

  def union[A](list: List[Set[A]]): Set[A] = list.foldRight(Set.empty[A])(_ union _)

  // they all need an initial value and a combining function
  trait Monoid[A] {
    def empty: A

    def combine(x: A, y: A): A
  }
}

// TODO: type classes vs. subtyping
// https://typelevel.org/cats/typeclasses.html#type-classes-vs-subtyping

// IMPLICIT DERIVATION - Monoid[Pair[A,B]] given Monoid[A] and Monoid[B]

import cats.Monoid

final case class Pair[A, B](a: A, b: B)

object Pair {
  implicit def tuple2Instance[A, B](implicit monA: Monoid[A], monB: Monoid[B]): Monoid[Pair[A, B]] =
    new Monoid[Pair[A, B]] {
      def empty: Pair[A, B] = Pair(monA.empty, monB.empty)

      def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
        Pair(monA.combine(x.a, y.a), monB.combine(x.b, y.b))
    }
}

// and then combine stuffs!

implicit val intMon: Monoid[Int] = new Monoid[Int] {
  override def empty: Int = 0

  override def combine(x: Int, y: Int): Int = x + y
}

implicit val stringMon: Monoid[String] = new Monoid[String] {
  override def empty: String = ""

  override def combine(x: String, y: String): String = x ++ y
}

def combineAll[A](list: List[A])(implicit m: Monoid[A]): A = list.foldRight(m.empty)(m.combine)

combineAll(List(1, 2, 3, 4))
combineAll(List("1", "2", "3", "4"))
combineAll(List(Pair("1", 1), Pair("2", 2), Pair("3", 3), Pair("4", 4)))

/*
val res0: Int = 10
val res1: String = 1234
val res2: Pair[String,Int] = Pair(1234,10)
* */

object SyntaxThings {
  // i wanna write a ..
  def combineAll[A : Monoid](list: List[A]): A = ???

  // this is in standard library:
  def implicitly[A](implicit ev: A): A = ev

  // but this is efforts for the implementor ...
  def combineAll[A : Monoid](list: List[A]): A =
    list.foldRight(implicitly[Monoid[A]].empty)(implicitly[Monoid[A]].combine)

  // so typeclass libraries (e.g. cats) helpfully gives us something like
  /*
  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }
   */

  // so then u can jus
  def combineAll[A : Monoid](list: List[A]): A =
    list.foldRight(Monoid[A].empty)(Monoid[A].combine)

}

// Laws
// e.g. you don't want to implement empty and combine any whatever way do u
// Monoid:
// empty: must be identity element
// combine: must be associative
// TODO: look at some law testing modules