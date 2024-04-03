package types

import cats.{Functor, Monoid, Semigroup}

final case class Last[+T](value: Option[T])

object Last {
  implicit def dummySemigroup[A]: Semigroup[Last[A]] = ???
  implicit def dummyMonoid[A]: Monoid[Last[A]]       = ???
  implicit def dummyFunctor[A]: Functor[Last]        = ???
}
