package types

import cats.{Functor, Monoid, Semigroup}

final case class Point[+T](x: T, y: T, z: T)

object Point {
  implicit def dummySemigroup[A]: Semigroup[Point[A]] = ???
  implicit def dummyMonoid[A]: Monoid[Point[A]]       = ???
  implicit def dummyFunctor[A]: Functor[Point]        = ???
}
