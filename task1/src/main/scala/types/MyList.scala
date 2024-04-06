package types

import cats.Monad

import scala.annotation.tailrec

sealed trait MyList[+T] {
  def add[A >: T](value: A): MyList[A]

  def headOption: Option[T]

  def elementOption(position: Int): Option[T]

}

case object MyNil extends MyList[Nothing] {
  override def add[A >: Nothing](value: A): MyList[A] = MyListBody(value, MyNil)

  override def elementOption(position: Int): Option[Nothing] = None

  override def headOption: Option[Nothing] = None

}

case class MyListBody[+T](head: T, tail: MyList[T]) extends MyList[T] {
  override def add[A >: T](value: A): MyList[A] = MyListBody(value, MyListBody(head, tail))

  override def headOption: Option[T] = Some(head)

  final override def elementOption(position: Int): Option[T] = {
    @tailrec
    def loop(myList: MyList[T], position: Int): Option[T] = {
      position match {
        case 0 => myList.headOption
        case value if value > 0 =>
          myList match {
            case MyNil               => None
            case MyListBody(_, tail) => loop(tail, position - 1)
          }
        case _ => None
      }
    }

    loop(MyListBody(head, tail), position)
  }
}

object MyList {

  def apply[A](args: A*): MyList[A] =
    args.reverse.foldLeft(MyList.empty[A])((z, x) => MyListBody(x, z))

  def empty[A]: MyList[A] = MyNil

  def single[A](value: A): MyList[A] = MyListBody(value, MyNil)

  private def concat[A](left: MyList[A], right: MyList[A]): MyList[A] =
    left match {
      case MyNil                  => right
      case MyListBody(head, tail) => MyListBody(head, concat(tail, right))
    }

  implicit val myListMonad: Monad[MyList] =
    new Monad[MyList] {

      override def pure[A](x: A): MyList[A] = MyListBody(x, MyNil)

      override def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] =
        fa match {
          case MyNil                  => MyNil
          case MyListBody(head, tail) => concat(f(head), flatMap(tail)(f))
        }

      override def tailRecM[A, B](a: A)(f: A => MyList[Either[A, B]]): MyList[B] = {

        @tailrec
        def loop(remaining: MyList[MyList[Either[A, B]]], acc: MyList[B]): MyList[B] =
          remaining match {
            case MyNil => acc
            case MyListBody(head, tail) =>
              head match {
                case MyNil => loop(tail, acc)
                case MyListBody(inHead, inTail) =>
                  inHead match {
                    case Left(value) =>
                      loop(concat(pure(concat(f(value), inTail)), tail), acc)
                    case Right(value) =>
                      loop(concat(pure(inTail), tail), acc.add(value))
                  }
              }
          }

        loop(pure(f(a)), empty)
      }
    }
}
