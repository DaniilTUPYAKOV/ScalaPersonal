package types

import cats.implicits.{catsSyntaxSemigroup, toFunctorOps}
import cats.kernel.Monoid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LastSpec extends AnyFlatSpec with Matchers {
  "Last" should "have monoid instance" in {
    empty[Nothing] shouldBe Last(None)
  }

  it should "right combine with empty" in {
    Last(Some(1)) |+| empty shouldBe Last(Some(1))
  }

  it should "left combine with empty" in {
    empty[Int] |+| Last(Some(1)) shouldBe Last(Some(1))
  }

  it should "combine with non empty" in {
    Last(Some(1)) |+| Last(Some(2)) shouldBe Last(Some(2))
  }

  "Semigroup for Last" should "be associative" in {
    Last(Some(1)) |+| Last(Some(2)) |+| Last(Some(3)) shouldBe Last(Some(1)) |+|
      (Last(Some(2)) |+| Last(Some(3)))
  }

  "Functor for Last" should "have instance" in {
    Last(Some(1)).map(_ * 2) shouldBe Last(Some(2))
  }

  it should "work with empty" in {
    empty.map(identity) shouldBe empty
  }

  it should "identity" in {
    Last(Some(3)).map(identity) shouldBe Last(Some(3))
  }

  it should "compose" in {
    def mul(num: Int): Int = num * 2
    def div(num: Int): Int = num / 2

    Last(Some(3)).map(mul).map(div) shouldBe Last(Some(3)).map(num => div(mul(num)))
  }

  private def empty[T]: Last[T] =
    Monoid[Last[T]].empty
}
