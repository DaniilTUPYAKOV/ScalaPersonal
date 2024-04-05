package types

import cats.kernel.Eq
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class MyListLawSpec extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  checkAll(
    name = "RecoverWith.MonadLaws",
    ruleSet = MonadTests[MyList].monad[Int, Int, String]
  )

  implicit def eqTree[A: Eq]: Eq[MyList[A]] = Eq.fromUniversalEquals

  implicit def arbTree[A: Arbitrary]: Arbitrary[MyList[A]] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(MyNil),
        Arbitrary.arbitrary[A].map(MyList.single)
      )
    )
}
