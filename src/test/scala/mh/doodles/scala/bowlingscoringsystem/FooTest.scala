package mh.doodles.scala.bowlingscoringsystem

import org.scalatest._
import scala.collection.immutable.Seq

class FooTest extends fixture.FunSpec with Matchers {

  type FixtureParam = scala.collection.mutable.Stack[Seq[(Seq[Seq[Int]], Int)]]

  def withFixture(test: OneArgTest): Outcome = {
    val testDataNoBonuses: Seq[(Seq[Seq[Int]], Int)] = Seq[(Seq[Seq[Int]], Int)](
      (Seq.fill(10)(Seq(0, 0)), 0),
      (Seq.fill(10)(Seq(1, 0)), 10),
      (Seq.fill(10)(Seq(0, 1)), 10),
      (Seq.fill(10)(Seq(3, 0)), 30),
      (Seq.fill(10)(Seq(0, 3)), 30)
    )

    val testDataBonuses = Seq[(Seq[Seq[Int]], Int)](
      (Seq(Seq(5, 5), Seq(3, 3), Seq(5, 5), Seq(4, 4), Seq(5, 5), Seq(2, 2), Seq(5, 5), Seq(1, 1), Seq(5, 5), Seq(5, 5, 1)), 96),
      (Seq(Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5), Seq(5, 5, 5)), 150),
      (Seq(Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3), Seq(7, 3, 7)), 170),
      (Seq(Seq(10), Seq(9, 1), Seq(10), Seq(10), Seq(10), Seq(8, 2), Seq(10), Seq(8, 0), Seq(5, 5), Seq(10, 10, 10)), 214)
    )

    val testDataStrikes: Seq[(Seq[Seq[Int]], Int)] = Seq[(Seq[Seq[Int]], Int)](
      (Seq.fill(12)(Seq(10)), 180),
      (0 to 10 map { x => if (x % 3 == 0) Seq(10) else Seq(3, 2) }, 15),
      (0 to 10 map { x => if (x % 2 == 0) Seq(10) else Seq(7, 0) }, 35)
    )

    val testDataSpares: Seq[(Seq[Seq[Int]], Int)] = Seq[(Seq[Seq[Int]], Int)](
      (Seq.fill(10)(Seq(7, 3)), 63),
      (0 to 10 map { x => if (x % 2 == 0) Seq(5, 5) else Seq(0, 3) }, 0),
      (0 to 10 map { x => if (x % 2 == 0) Seq(5, 5) else Seq(0, 3) }, 0),
      (0 to 10 map { x => if (x % 2 == 0) Seq(5, 5) else Seq(3, 3) }, 15),
      (0 to 10 map { x => if (x % 2 == 0) Seq(5, 5) else Seq(8, 1) }, 40)
    )

    val stack = new scala.collection.mutable.Stack[Seq[(Seq[Seq[Int]], Int)]]
    stack.push(testDataSpares)
    stack.push(testDataStrikes)
    stack.push(testDataBonuses)
    stack.push(testDataNoBonuses)
    test(stack)
  }

  describe("class Foo") {
    describe(s"pass the following scenarios") {
      it(s"calculate games without bonuses successfully") { fixtures =>
        val fooObj = new Foo()
        fixtures.head.foreach { testData =>
          fooObj.calculateTotal(testData._1) shouldBe testData._2
        }
      }

      it(s"calculate games with bonuses successfully") { fixtures =>
        val fooObj = new Foo()
        fixtures(1).foreach { testData =>
          fooObj.calculateTotal(testData._1) shouldBe testData._2
        }
      }

      it(s"calculate games with strikes successfully") { fixtures =>
        val fooObj = new Foo()
        fixtures(2).foreach { testData =>
          fooObj.calculateStrikeBonus(testData._1) shouldBe testData._2
        }
      }

      it(s"calculate games with spares successfully") { fixtures =>
        val fooObj = new Foo()
        fixtures(3).foreach { testData =>
          fooObj.calculateSpareBonus(testData._1) shouldBe testData._2
        }
      }
    }
  }

}
