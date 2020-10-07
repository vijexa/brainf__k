package brainfk

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import brainfk.Main._

class MainSpec extends AnyFlatSpec with should.Matchers {
  {
    val pg1 = ProgramData("", "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")
    val pg2 = ProgramData("pm$", "++[>,+++.<-]+[]")

    {
      import Executor._

      "Executor.findClose" should "find closing ']' bracket after specified index" in {
        findClose(pg1.code indexOf '[', pg1) shouldBe Some(pg1.code indexOf ']')
        findClose(pg2.code indexOf '[', pg2) shouldBe Some(pg2.code indexOf ']')
        findClose(13, pg2) shouldBe Some(14)
      }

      "Executor.findClose" should "return None when there is no ']' after index" in {
        findClose(50, pg1) shouldBe None
        findClose(14, pg2) shouldBe None
        findClose(pg2.code.length, pg2) shouldBe None
      }

      "Executor.findOpen" should "find opening '[' bracket before specified index" in {
        findOpen(pg1.code indexOf ']', pg1) shouldBe Some(pg1.code indexOf '[')
        findOpen(pg2.code indexOf ']', pg2) shouldBe Some(pg2.code indexOf '[')
        findOpen(14, pg2) shouldBe Some(13)
      }
      
      "Executor.findOpen" should "return None when there is no '[' before index" in {
        findOpen(5, pg1) shouldBe None
        findOpen(0, pg1) shouldBe None
        findOpen(2, pg2) shouldBe None
      }
    }

    {
      import ProgramChecker._

      val invpg11 = ProgramData("", "++++++++++>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")
      val invpg12 = ProgramData("", "++++++++++[>+++++++>++++++++++>+++>+<<<<->++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")

      val pg21 = ProgramData("pm$", "++[>,+++.<-]+]")

      val invpg21 = ProgramData("pm$", "++>,+++.<-]+[]")
      val invpg22 = ProgramData("pm$", "++[>,+++.<-]+[")

      "ProgramChecker.checkBrackets" should "return Right(ProgramData)" in {
        checkBrackets(pg1) shouldBe Right(pg1)
        checkBrackets(pg2) shouldBe Right(pg2)
        checkBrackets(pg21) shouldBe Right(pg21)
      }

      "ProgramChecker.checkBrackets" should "return Left(ErrorMessage)" in {
        checkBrackets(invpg11) shouldBe Left(ErrorMessage("Found excess ']' at 40"))
        checkBrackets(invpg12) shouldBe Left(ErrorMessage("Found excess '[' at 10"))

        checkBrackets(invpg21) shouldBe Left(ErrorMessage("Found excess ']' at 10"))
        checkBrackets(invpg22) shouldBe Left(ErrorMessage("Found excess '[' at 13"))
      }
    }

  }
}
