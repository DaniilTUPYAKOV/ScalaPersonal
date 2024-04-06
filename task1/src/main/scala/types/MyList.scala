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

  @tailrec
  private def toList[T](myList: MyList[T], acc: List[T]): List[T] =
    myList match {
      case MyNil                  => acc
      case MyListBody(head, tail) => toList(tail, head :: acc)
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
      case that: MyListBody[T] => compare(MyListBody(head, tail), that)
      case _                   => false
    }

}

object MyList {

  def apply[A](args: A*): MyList[A] =
    args.reverse.foldLeft(MyList.empty[A])((z, x) => MyListBody(x, z))

  def empty[A]: MyList[A] = MyNil

  def single[A](value: A): MyList[A] = MyListBody(value, MyNil)

  @tailrec
  private def reverse[A](list: MyList[A], acc: MyList[A]): MyList[A] =
    list match {
      case MyNil                  => acc
      case MyListBody(head, tail) => reverse(tail, MyListBody(head, acc))
    }

  private def concat[A](left: MyList[A], right: MyList[A]): MyList[A] = {

    @tailrec
    def loop(list1: MyList[A], acc: MyList[A]): MyList[A] =
      list1 match {
        case MyNil                  => acc
        case MyListBody(head, tail) => loop(tail, MyListBody(head, acc))
      }

    loop(reverse(left, MyNil), right)
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

        reverse(loop(pure(f(a)), empty), MyNil)
      }
    }
}
