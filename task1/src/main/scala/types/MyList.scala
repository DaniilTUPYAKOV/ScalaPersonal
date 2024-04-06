package types

import cats.Monad

import scala.annotation.tailrec

sealed trait MyList[+T] {
  def add[A >: T](value: A): MyList[A]

  def headOption: Option[T]

  def elementOption(position: Int): Option[T]

  def reverse: MyList[T]

  def mySelf: MyList[T]

}

case object MyNil extends MyList[Nothing] {
  override def add[A >: Nothing](value: A): MyList[A] = MyListBody(value, MyNil)

  override def elementOption(position: Int): Option[Nothing] = None

  override def headOption: Option[Nothing] = None

  override def reverse: MyList[Nothing] = MyNil

  override def mySelf: MyList[Nothing] = MyNil
}

case class MyListBody[+T](head: T, tail: MyList[T]) extends MyList[T] {

  override def add[A >: T](value: A): MyList[A] = MyListBody(value, MyListBody(head, tail))

  override def headOption: Option[T] = Some(head)

  override def mySelf: MyList[T] = MyListBody(head, tail)

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

    loop(mySelf, position)
  }

  @tailrec
  private def compare[T](list1: MyList[T], list2: MyList[T]): Boolean =
    (list1, list2) match {
      case (MyListBody(head1, tail1), MyListBody(head2, tail2)) =>
        (tail1, tail2) match {
          case (MyNil, MyNil) if head1 == head2                         => true
          case (t1: MyListBody[T], t2: MyListBody[T]) if head1 == head2 => compare(t1, t2)
          case _                                                        => false
        }
      case _ => false
    }

  override def equals(obj: Any): Boolean =
    obj match {
      case that: MyListBody[T] => compare(mySelf, that)
      case _                   => false
    }

  override def reverse: MyList[T] = {
    @tailrec
    def loop(remaining: MyList[T], acc: MyList[T]): MyList[T] = {
      remaining match {
        case MyNil => acc
        case MyListBody(head, tail) => loop(tail, MyListBody(head, acc))
      }
    }
    loop(mySelf, MyNil)
  }
}

object MyList {

  def apply[A](args: A*): MyList[A] =
    args.reverse.foldLeft(MyList.empty[A])((z, x) => MyListBody(x, z))

  def empty[A]: MyList[A] = MyNil

  def single[A](value: A): MyList[A] = MyListBody(value, MyNil)

  private def concat[A](left: MyList[A], right: MyList[A]): MyList[A] = {

    @tailrec
    def loop(list1: MyList[A], acc: MyList[A]): MyList[A] =
      list1 match {
        case MyNil                  => acc
        case MyListBody(head, tail) => loop(tail, MyListBody(head, acc))
      }

    loop(left.reverse, right)
  }

  implicit val myListMonad: Monad[MyList] =
    new Monad[MyList] {

      override def pure[A](x: A): MyList[A] = MyListBody(x, MyNil)

      override def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = {

        @tailrec
        def loop(remaining: MyList[A], acc: MyList[B]): MyList[B] = {
          remaining match {
            case MyNil                  => acc
            case MyListBody(head, tail) => loop(tail, concat(acc, f(head)))
          }
        }
        loop(fa, MyNil)
      }

      override def tailRecM[A, B](a: A)(f: A => MyList[Either[A, B]]): MyList[B] = {

        @tailrec
        def loop(remaining: MyList[MyList[Either[A, B]]], acc: MyList[B]): MyList[B] =
          remaining match {
            case MyNil                   => acc
            case MyListBody(MyNil, tail) => loop(tail, acc)
            case MyListBody(MyListBody(inHead, inTail), tail) =>
              inHead match {
                case Left(value) =>
                  loop(
                    concat(pure(concat(f(value), inTail)), tail),
                    acc
                  )
                case Right(value) =>
                  loop(
                    concat(pure(inTail), tail),
                    acc.add(value)
                  )
              }
          }

        loop(pure(f(a)), empty).reverse
      }
    }
}
