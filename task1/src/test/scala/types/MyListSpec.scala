package types

import cats.Monad
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps}

class MyListSpec extends AnyFlatSpec with Matchers {
  "MyList" should "have Empty implementation" in {
    MyList.empty shouldBe MyNil
  }

  it should "add Int elements" in {
    MyList.empty.add(1).add(2) shouldBe MyListBody(2, MyListBody(1, MyNil))
  }

  it should "add String elements" in {
    MyList.empty.add("hello").add("hey") shouldBe MyListBody("hey", MyListBody("hello", MyNil))
  }

  it should "add Box[A] elements" in {
    MyList.empty.add(Some(1)).add(Some(4)) shouldBe MyListBody(Some(4), MyListBody(Some(1), MyNil))
  }

  it should "add another MyList elements" in {
    MyList.empty.add(MyList.empty.add(1)) shouldBe MyListBody(MyListBody(1, MyNil), MyNil)
  }

  it should "created by sequence" in {
    MyList(1, 2, 3) shouldBe MyList.empty.add(3).add(2).add(1)
    MyList("one", "two", "three") shouldBe MyList.empty.add("three").add("two").add("one")
    MyList(1, "two", MyList("three")) shouldBe MyList.empty.add(MyList("three")).add("two").add(1)
  }

  "Monad for MyList" should "pure method works" in {
    1.pure[MyList] shouldBe MyList.empty.add(1)
  }

  it should "flatMap method works for Empty" in {
    val testEmptyList: MyList[Int] = MyList.empty

    testEmptyList.flatMap(x => (x * 2).pure[MyList]) shouldBe MyList.empty
  }

  it should "flatMap method works for MyList[Int] with Int => MyList[Int] with singe value" in {
    val testIntList: MyList[Int] = Monad[MyList].pure(1)

    testIntList.flatMap(x => (x * 2).pure[MyList]) shouldBe MyList.empty.add(2)
  }

  it should "flatMap method works for MyList[Int] with Int => MyList[Int] with multiple value" in {
    val testIntList: MyList[Int] = 1.pure[MyList].add(2)

    testIntList
      .flatMap(x => (x * 2).pure[MyList].add(x * 3).add(x * 4)) shouldBe MyList
      .empty.add(2).add(3).add(4).add(4).add(6).add(8)
  }

  it should "be left identity" in {
    def mul(num: Int): MyList[Int] = (num * 2).pure[MyList].add(num * 3).add(num * 4)
    def plus(num: Int): MyList[Int] = (num + 2).pure[MyList].add(num + 3).add(num + 4)

    1.pure[MyList].flatMap(x => (x * 2).pure[MyList]) shouldBe 2.pure[MyList]

    1.pure[MyList].flatMap(mul) shouldBe mul(1)
    2.pure[MyList].flatMap(mul) shouldBe mul(2)

    1.pure[MyList].flatMap(plus) shouldBe plus(1)
    2.pure[MyList].flatMap(plus) shouldBe plus(2)
  }

  it should "be right identity" in {
    val testList: MyList[Int] = 1.pure[MyList].add(2).add(3)

    testList.flatMap(_.pure[MyList]) shouldBe testList
  }

  it should "be associativity" in {
    def f(num: Int): MyList[Int] = (num * 2).pure[MyList].add(num * 3).add(num * 4)
    def g(num: Int): MyList[Int] = (num + 2).pure[MyList].add(num + 3).add(num + 4)
    val testList: MyList[Int] = 1.pure[MyList].add(2).add(3)

    testList.flatMap(f).flatMap(g) shouldBe testList.flatMap(x => f(x).flatMap(g))
  }

}
