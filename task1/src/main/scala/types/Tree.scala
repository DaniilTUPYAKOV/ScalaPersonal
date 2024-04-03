package types

import cats.{Functor, Monoid, Semigroup}

sealed trait Tree[+T]

object Tree {
  final case class Leaf[+T](value: T)                                  extends Tree[T]
  final case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
  object Nil                                                           extends Tree[Nothing]

  implicit def dummySemigroup[A]: Semigroup[Tree[A]] = ???
  implicit def dummyMonoid[A]: Monoid[Tree[A]]       = ???
  implicit def dummyFunctor[A]: Functor[Tree]        = ???
}
