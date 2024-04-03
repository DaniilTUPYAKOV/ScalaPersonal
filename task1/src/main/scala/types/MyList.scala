package types

import cats.Monad

import scala.annotation.tailrec

sealed trait MyList[+T] {
  def add[A >: T](value: A): MyList[A]
}

case object MyNil extends MyList[Nothing] {
  override def add[A >: Nothing](value: A): MyList[A] = MyListBody(value, MyNil)

}

case class MyListBody[+T](head: T, tail: MyList[T]) extends MyList[T] {
  override def add[A >: T](value: A): MyList[A] = MyListBody(value, MyListBody(head, tail))
}

object MyList {

  def empty: MyList[Nothing] = MyNil

  def concat[A](left: MyList[A], right: MyList[A]): MyList[A] =
    left match {
      case MyNil => right
      case MyListBody(head, tail) => MyListBody(head, concat(tail, right))
    }

  implicit val myListMonad: Monad[MyList] =
    new Monad[MyList] {

      override def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] =
        fa match {
          case MyNil => MyNil
          case MyListBody(head, tail) => concat(f(head), flatMap(tail)(f))
        }

      override def tailRecM[A, B](a: A)(f: A => MyList[Either[A, B]]): MyList[B] = {
        flatMap(f(a)){
          case Left(value) => tailRecM(value)(f)
          case Right(value) => MyListBody(value, MyNil)
        }
      }

      override def pure[A](x: A): MyList[A] = MyListBody(x, MyNil)
    }

}

