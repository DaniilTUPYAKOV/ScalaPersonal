package types

import cats.Monad
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.applicative._

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

  "Monad for MyList" should "pure method works" in {
    Monad[MyList].pure(1) shouldBe MyList.empty.add(1)
  }

  it should "flatMap method works for Empty" in {
    val testEmptyList: MyList[Int] = MyList.empty
    Monad[MyList].flatMap(testEmptyList)(x => Monad[MyList].pure(x * 2)) shouldBe MyList.empty
  }

  it should "flatMap method works for MyList[Int] with Int => MyList[Int] with singe value" in {
    val testIntList: MyList[Int] = Monad[MyList].pure(1)
    Monad[MyList].flatMap(testIntList)(x => Monad[MyList].pure(x * 2)) shouldBe MyList.empty.add(2)
  }

  it should "flatMap method works for MyList[Int] with Int => MyList[Int] with multiple value" in {
    val testIntList: MyList[Int] =
      Monad[MyList]
        .pure(1)
        .add(2)

    Monad[MyList]
      .flatMap(testIntList)(x => Monad[MyList].pure(x * 2).add(x * 3).add(x * 4)) shouldBe MyList
      .empty.add(2).add(3).add(4).add(4).add(6).add(8)
  }

  it should "be left identity" in {
    def mul(num: Int): MyList[Int] = (num * 2).pure[MyList].add(num * 3).add(num * 4)
    def plus(num: Int): MyList[Int] = (num + 2).pure[MyList].add(num + 3).add(num + 4)

    Monad[MyList].flatMap(1.pure[MyList])(x => (x*2).pure[MyList]) shouldBe 2.pure[MyList]

    Monad[MyList].flatMap(1.pure[MyList])(mul) shouldBe mul(1)
    Monad[MyList].flatMap(2.pure[MyList])(mul) shouldBe mul(2)

    Monad[MyList].flatMap(1.pure[MyList])(plus) shouldBe plus(1)
    Monad[MyList].flatMap(2.pure[MyList])(plus) shouldBe plus(2)
  }

  it should "be right identity" in {
    val testList: MyList[Int] = 1.pure[MyList].add(2).add(3)
    Monad[MyList].flatMap(testList)(Monad[MyList].pure) shouldBe testList
  }

  it should "be associativity" in {
    def f(num: Int): MyList[Int] = (num * 2).pure[MyList].add(num * 3).add(num * 4)
    def g(num: Int): MyList[Int] = (num + 2).pure[MyList].add(num + 3).add(num + 4)
    val testList: MyList[Int] = 1.pure[MyList].add(2).add(3)

    Monad[MyList]
      .flatMap(Monad[MyList].flatMap(testList)(f))(g) shouldBe Monad[MyList]
      .flatMap(testList)(x => Monad[MyList].flatMap(f(x))(g))
  }

}
