package types

import cats.Monoid
import cats.implicits.{catsSyntaxSemigroup, toFunctorOps}
import cats.kernel.Semigroup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointSpec extends AnyFlatSpec with Matchers {
  "Semigroup for Point" should "have instance" in {
    Point(1, 2, 3) |+| Point(3, 4, 5) shouldBe Point[Int](4, 6, 8)
  }

  it should "have instance for custom types" in {
    Point(one, one, one) |+| Point(two, two, two) shouldBe Point(three, three, three)
  }

  it should "be associative" in {
    Point(1, 2, 3) |+| Point(3, 4, 5) |+| Point(3, 4, 5) shouldBe Point(1, 2, 3) |+|
      (Point(3, 4, 5) |+| Point(3, 4, 5))
  }

  "Monoid for Point" should "have instance" in {
    empty[Int] shouldBe Point(0, 0, 0)
  }

  it should "left identity" in {
    empty[Int] |+| Point(3, 2, 1) shouldBe Point(3, 2, 1)
  }

  it should "right identity" in {
    Point(3, 2, 1) |+| empty[Int] shouldBe Point(3, 2, 1)
  }

  "Functor for Point" should "have instance" in {
    Point(1, 2, 3).map(_ * 2) shouldBe Point[Int](2, 4, 6)
  }

  it should "identity" in {
    Point(1, 1, 2).map(identity) shouldBe Point(1, 1, 2)
  }

  it should "compose" in {
    def mul(num: Int): Int = num * 2
    def div(num: Int): Int = num / 2

    Point(1, 1, 2).map(mul).map(div) shouldBe Point(1, 1, 2).map(num => div(mul(num)))
  }

  private def empty[T: Monoid]: Point[T] = Monoid[Point[T]].empty
  private lazy val one                   = PositiveInt(1)
  private lazy val two                   = PositiveInt(2)
  private lazy val three                 = PositiveInt(3)

  case class PositiveInt(number: Int)

  private implicit val positiveIntSemigroup: Semigroup[PositiveInt] = {
    case (PositiveInt(x), PositiveInt(y)) => PositiveInt(x + y)
  }
}
