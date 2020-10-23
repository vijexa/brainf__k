package brainfk

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import brainfk.Main._

import cats._
import cats.implicits._
import scala.util.chaining._

class MainSpec extends AnyFlatSpec with should.Matchers {
  {
    val pg1 = ProgramData("", "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")
    val pg2 = ProgramData("pm$", "++[>,+++.<-]+[]")

    {
      import Compiler._

      "Compiler.findClose" should "find closing ']' bracket after specified index" in {
        findClose(pg1.code indexOf '[', pg1.code) shouldBe Some(pg1.code indexOf ']')
        findClose(pg2.code indexOf '[', pg2.code) shouldBe Some(pg2.code indexOf ']')
        findClose(13, pg2.code) shouldBe Some(14)
      }

      "Compiler.findClose" should "return None when there is no ']' after index" in {
        findClose(50, pg1.code) shouldBe None
        findClose(14, pg2.code) shouldBe None
        findClose(pg2.code.length, pg2.code) shouldBe None
      }

      "Compiler.findOpen" should "find opening '[' bracket before specified index" in {
        findOpen(pg1.code indexOf ']', pg1.code) shouldBe Some(pg1.code indexOf '[')
        findOpen(pg2.code indexOf ']', pg2.code) shouldBe Some(pg2.code indexOf '[')
        findOpen(14, pg2.code) shouldBe Some(13)
      }
      
      "Compiler.findOpen" should "return None when there is no '[' before index" in {
        findOpen(5, pg1.code) shouldBe None
        findOpen(0, pg1.code) shouldBe None
        findOpen(2, pg2.code) shouldBe None
      }
    }

    {
      import ProgramChecker._

      val invpg11 = ProgramData("", "++++++++++>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")
      val invpg12 = ProgramData("", "++++++++++[>+++++++>++++++++++>+++>+<<<<->++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")

      val pg21 = ProgramData("pm$", "++[>,+++.<-]+]")
      val pg22 = ProgramData("pm$", "++>,+++.<-+")

      val invpg21 = ProgramData("pm$", "++>,+++.<-]+[]")
      val invpg22 = ProgramData("pm$", "++[>,+++.<-]+[")
      val invpg23 = ProgramData("pm$", "]++[>,+++.<-]+")

      "ProgramChecker.checkBrackets" should "return Right(ProgramData)" in {
        checkBrackets(pg1) shouldBe Right(pg1)
        checkBrackets(pg2) shouldBe Right(pg2)
        checkBrackets(pg21) shouldBe Right(pg21)
        checkBrackets(pg22) shouldBe Right(pg22)
      }

      "ProgramChecker.checkBrackets" should "return Left(ErrorMessage)" in {
        checkBrackets(invpg11) shouldBe Left(ErrorMessage("Found excess ']' at 40"))
        checkBrackets(invpg12) shouldBe Left(ErrorMessage("Found excess '[' at 10"))

        checkBrackets(invpg21) shouldBe Left(ErrorMessage("Found excess ']' at 10"))
        checkBrackets(invpg22) shouldBe Left(ErrorMessage("Found excess '[' at 13"))
        checkBrackets(invpg23) shouldBe Left(ErrorMessage("Found excess ']' at 0"))
      }
    }

    {
      import Command._

      val pg1Compiled = CompiledProgram(pg1.input, Vector(IncCell(10), OpenBracket(1,12), IncMemPtr(1), IncCell(7), IncMemPtr(1), IncCell(10), IncMemPtr(1), IncCell(3), IncMemPtr(1), IncCell(1), DecMemPtr(4), DecCell(1), CloseBracket(1,1), IncMemPtr(1), IncCell(2), OutChar(1), IncMemPtr(1), IncCell(1), OutChar(1), IncCell(7), OutChar(2), IncCell(3), OutChar(1), IncMemPtr(1), IncCell(2), OutChar(1), DecMemPtr(2), IncCell(15), OutChar(1), IncMemPtr(1), OutChar(1), IncCell(3), OutChar(1), DecCell(6), OutChar(1), DecCell(8), OutChar(1), IncMemPtr(1), IncCell(1), OutChar(1)))
      val pg2Compiled = CompiledProgram(pg2.input, Vector(IncCell(2), OpenBracket(1, 8), IncMemPtr(1), InChar(1), IncCell(3), OutChar(1), DecMemPtr(1), DecCell(1), CloseBracket(1, 1), IncCell(1), OpenBracket(1, 11), CloseBracket(1, 10)))

      "Compiler.compile" should "correctly compile program" in {
        import Compiler._

        // "++[>,+++.<-]+[]"
        Compiler.compile(pg1) shouldBe pg1Compiled
        Compiler.compile(pg2) shouldBe pg2Compiled
      }

      "Executor.runPg" should "return correct output from program execution" in {
        import Executor._

        val pg1Output = OutputData("Hello World!").asRight
        val pg2Output = ErrorMessage("PROCESS TIME OUT. KILLED!!!", OutputData("sp").some).asLeft

        runPg(pg1Compiled) shouldBe pg1Output
        runPg(pg2Compiled) shouldBe pg2Output
      }

    }

  }

  "Program" should "correctly handle edge cases" in {
      import ProgramChecker._
      import Compiler._
      import Executor._

      def run (pg: ProgramData) = 
        for {
          _         <- ProgramChecker.checkBrackets(pg)
          compiled  <- Compiler.compile(pg).asRight
          results   <- Executor.runPg(compiled)
        } yield results 

      run(ProgramData("abcde", ",,,.,,.")) shouldBe OutputData("ce").asRight

      run(ProgramData("abc", ",,,,.")) shouldBe ErrorMessage("End of input string reached at 3!", OutputData("").some).asLeft
    }
}
