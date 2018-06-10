package abc

import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SBotTest extends FunSuite with Matchers {

  test("Various lists should be ok") {
    List() shouldBe Nil
    List(1, 2, 3) shouldNot be ("empty")
    List("fee", "fie", "foe", "fum") should have length 4
  }

  test("A set does not depend on the permutation") {
    Set(1, 2, 3) shouldBe Set(3, 2, 1)
  }

}
