package types

import cats.implicits.{catsSyntaxSemigroup, toFunctorOps}
import cats.kernel.Monoid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import types.Tree.{Branch, Leaf, Nil}

class TreeSpec extends AnyFlatSpec with Matchers {
  "Semigroup for Tree" should "be associative for branch" in {
    branchOne |+| branchTwo |+| branchThree shouldBe branchOne |+| (branchTwo |+| branchThree)
    branchOne |+| branchThree |+| branchTwo shouldBe branchOne |+| (branchThree |+| branchTwo)
    branchTwo |+| branchThree |+| branchOne shouldBe branchTwo |+| (branchThree |+| branchOne)
    branchTwo |+| branchOne |+| branchThree shouldBe branchTwo |+| (branchOne |+| branchThree)
    branchThree |+| branchOne |+| branchTwo shouldBe branchThree |+| (branchOne |+| branchTwo)
    branchThree |+| branchTwo |+| branchOne shouldBe branchThree |+| (branchTwo |+| branchOne)
  }

  it should "be associative for leaf" in {
    leafOne |+| leafTwo |+| leafThree shouldBe leafOne |+| (leafTwo |+| leafThree)
  }

  it should "be associative for Nil" in {
    leafOne |+| leafTwo |+| Nil shouldBe leafOne |+| (leafTwo |+| Nil)
    branchOne |+| branchTwo |+| Nil shouldBe branchOne |+| (branchTwo |+| Nil)
  }

  it should "be associative for leaf and branch" in {
    leafOne |+| leafTwo |+| branchOne shouldBe leafOne |+| (leafTwo |+| branchOne)
    leafOne |+| branchTwo |+| leafThree shouldBe leafOne |+| (branchTwo |+| leafThree)
    branchThree |+| leafTwo |+| leafThree shouldBe branchThree |+| (leafTwo |+| leafThree)
    branchOne |+| leafTwo |+| branchTwo shouldBe branchOne |+| (leafTwo |+| branchTwo)
  }

  "Monoid for Tree" should "have monoid instance" in {
    empty[Nothing] shouldBe Nil
  }

  it should "right combine with empty" in {
    leafInt |+| empty shouldBe leafInt
  }

  it should "left combine with empty" in {
    empty[Int] |+| leafInt shouldBe leafInt
  }

  "Functor for Tree" should "have instance" in {
    branchOne.map(_ * 2) shouldBe Branch(2, Leaf(4), Leaf(6))
  }

  it should "change child branches" in {
    branchThree.map(_ * 3) shouldBe Branch(9, Branch(3, Leaf(6), Leaf(9)), Branch(6, Leaf(3), Nil))
  }

  it should "work with empty" in {
    empty.map(identity) shouldBe empty
  }

  it should "identity" in {
    branchOne.map(identity) shouldBe branchOne
    branchThree.map(identity) shouldBe branchThree
  }

  it should "compose" in {
    def mul(num: Int): Int = num * 2
    def div(num: Int): Int = num / 2

    branchThree.map(mul).map(div) shouldBe branchThree.map(num => div(mul(num)))
  }

  private def empty[T]: Tree[T] =
    Monoid[Tree[T]].empty

  private lazy val leafInt: Tree[Int]     = Leaf(1)
  private lazy val leafOne: Tree[Int]     = Leaf(1)
  private lazy val leafTwo: Tree[Int]     = Leaf(2)
  private lazy val leafThree: Tree[Int]   = Leaf(3)
  private lazy val branchOne: Tree[Int]   = Branch(1, leafTwo, leafThree)
  private lazy val branchTwo: Tree[Int]   = Branch(2, leafOne, Nil)
  private lazy val branchThree: Tree[Int] = Branch(3, branchOne, branchTwo)
}
