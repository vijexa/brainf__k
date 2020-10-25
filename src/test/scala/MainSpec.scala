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
    val pg3 = ProgramData("", "+++++++++++++++++++++++++++++++++++++++++++++++++[[[.>]]]")
    val pg4 = ProgramData("", "+++[++[-----]]++")
    val pg5 = ProgramData("", "+++[[---]++--]++")

    {
      import Validator._

      val invpg11 = ProgramData("", "++++++++++>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")
      val invpg12 = ProgramData("", "++++++++++[>+++++++>++++++++++>+++>+<<<<->++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.")

      val pg21 = ProgramData("pm$", "++>,+++.<-+")

      val invpg21 = ProgramData("pm$", "++[>,+++.<-]+]")
      val invpg22 = ProgramData("pm$", "++>,+++.<-]+[]")
      val invpg23 = ProgramData("pm$", "++[>,+++.<-]+[")
      val invpg24 = ProgramData("pm$", "]++[>,+++.<-]+")

      "ProgramChecker.checkBrackets" should "return Right(ProgramData)" in {
        checkBrackets(pg1) shouldBe pg1.asRight
        checkBrackets(pg2) shouldBe pg2.asRight
        checkBrackets(pg21) shouldBe pg21.asRight
      }

      "ProgramChecker.checkBrackets" should "return Left(ErrorMessage)" in {
        checkBrackets(invpg11) shouldBe ErrorMessage("Unmatched bracket detected!").asLeft
        checkBrackets(invpg12) shouldBe ErrorMessage("Unmatched bracket detected!").asLeft

        checkBrackets(invpg21) shouldBe ErrorMessage("Unmatched bracket detected!").asLeft
        checkBrackets(invpg22) shouldBe ErrorMessage("Unmatched bracket detected!").asLeft
        checkBrackets(invpg23) shouldBe ErrorMessage("Unmatched bracket detected!").asLeft
        checkBrackets(invpg24) shouldBe ErrorMessage("Unmatched bracket detected!").asLeft
      }
    }

    {
      import Command._

      val pg1Compiled = CompiledProgram(pg1.input, Vector(IncCell(10), OpenBracket(1,12), IncMemPtr(1), IncCell(7), IncMemPtr(1), IncCell(10), IncMemPtr(1), IncCell(3), IncMemPtr(1), IncCell(1), DecMemPtr(4), DecCell(1), CloseBracket(1,1), IncMemPtr(1), IncCell(2), OutChar(1), IncMemPtr(1), IncCell(1), OutChar(1), IncCell(7), OutChar(2), IncCell(3), OutChar(1), IncMemPtr(1), IncCell(2), OutChar(1), DecMemPtr(2), IncCell(15), OutChar(1), IncMemPtr(1), OutChar(1), IncCell(3), OutChar(1), DecCell(6), OutChar(1), DecCell(8), OutChar(1), IncMemPtr(1), IncCell(1), OutChar(1)))
      val pg2Compiled = CompiledProgram(pg2.input, Vector(IncCell(2), OpenBracket(1, 8), IncMemPtr(1), InChar(1), IncCell(3), OutChar(1), DecMemPtr(1), DecCell(1), CloseBracket(1, 1), IncCell(1), OpenBracket(1, 11), CloseBracket(1, 10)))
      val pg3Compiled = CompiledProgram(pg3.input, Vector(IncCell(49), OpenBracket(1,8), OpenBracket(1,7), OpenBracket(1,6), OutChar(1), IncMemPtr(1), CloseBracket(1,3), CloseBracket(1,2), CloseBracket(1,1)))
      // +++[++[-----]]++
      val pg4Compiled = CompiledProgram(pg4.input, Vector(IncCell(3), OpenBracket(1,6), IncCell(2), OpenBracket(1,5), DecCell(5), CloseBracket(1,3), CloseBracket(1,1), IncCell(2)))
      // +++[[---]++--]++
      val pg5Compiled = CompiledProgram(pg4.input, Vector(IncCell(3), OpenBracket(1,7), OpenBracket(1,4), DecCell(3), CloseBracket(1,2), IncCell(2), DecCell(2), CloseBracket(1,1), IncCell(2))) 

      "Compiler.compile" should "correctly compile program" in {
        import Compiler._

        // "++[>,+++.<-]+[]"
        Compiler.compile(pg1) shouldBe pg1Compiled
        Compiler.compile(pg2) shouldBe pg2Compiled
        Compiler.compile(pg3) shouldBe pg3Compiled
        Compiler.compile(pg4) shouldBe pg4Compiled
        Compiler.compile(pg5) shouldBe pg5Compiled
      }

      "Executor.runPg" should "return correct output from program execution" in {
        import Executor._

        val pg1Output = OutputData("Hello World!").asRight
        val pg2Output = ErrorMessage("PROCESS TIME OUT. KILLED!!!", OutputData("sp").some).asLeft
        val pg3Output = OutputData("1").asRight

        runPg(pg1Compiled) shouldBe pg1Output
        runPg(pg2Compiled) shouldBe pg2Output
        runPg(pg3Compiled) shouldBe pg3Output
      }

    }

  }

  "Program" should "correctly handle edge cases" in {
    import Validator._
    import Compiler._
    import Executor._

    def run (pg: ProgramData) = 
      for {
        _         <- Validator.checkBrackets(pg)
        compiled  <- Compiler.compile(pg).asRight
        results   <- Executor.runPg(compiled/*.tap(println)*/)
      } yield results 

    def strip (str: String) = 
      str filter (c => "<>+-.,[]" contains c)

    run(ProgramData("abcde", ",,,.,,.")) shouldBe OutputData("ce").asRight

    run(ProgramData("abc", ",,,,.")) shouldBe ErrorMessage("End of input string reached at 3!", OutputData("").some).asLeft



    run(
      ProgramData("", 
        strip(
          """
            ++++++++++ set up line feed
            >+++++ set up counter for lines
            >++++++++++++++++++++++++++++++++++++++++++++++++ go to digits
            < set pointer to line counter
            [
                >>++++++++++ set up counter for digits
                [
                    <.+>- print out digits from 0 to 9
                ] 
                <---------- reset digits
                <<. print out lf
                >- decrement line counter
            ]
          """
        )
      )
    ) shouldBe OutputData("0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n").asRight

    
    run(
      ProgramData("r", 
        strip(
          """
            ,                            Read a character
                -[                       Skip forward if character is 0
                    >>++++[>++++++++<-]  Set up divisor (32) for division loop
                                          (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)
                    <+<-[                Set up dividend (x minus 1) and enter division loop
                        >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward
                        <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient
                        <<<<<-           Decrement dividend
                    ]                    End division loop
                ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag
                >--[-[<->+++[-]]]<[         Zero that flag unless quotient was 2 or 3; zero quotient; check flag
                    ++++++++++++<[       If flag then set up divisor (13) for second division loop
                                          (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)
                        >-[>+>>]         Reduce divisor; Normal case: increase remainder
                        >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient
                        <<<<<-           Decrease dividend
                    ]                    End division loop
                    >>[<+>-]             Add remainder back to divisor to get a useful 13
                    >[                   Skip forward if quotient was 0
                        -[               Decrement quotient and skip forward if quotient was 1
                            -<<[-]>>     Zero quotient and divisor if quotient was 2
                        ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1
                    ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0
                ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)
                <[-]                     Clear remainder from first division if second division was skipped
                <.                       Output ROT13ed character from copy and clear it
          """
        )
      )
    ) shouldBe OutputData("d").asRight

    run(
      ProgramData("Conqueror of British Empire (CWE). All hail Idi Amin!!!",
        strip(
          """
            +++++
            [>
                ++++++++
                [>
                    ++++++++++
                    [
                        >
                        ++++++++++++
                        >
                        ++++++++++++
                        >
                        ++++++++++++
                        >
                        +++++++++++
                        >
                        +++++++++++
                        <<<<<
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    <<<<<
                    <-
                ]
                <-
            ]

            ++++++++++++++
            [
                >
                +++++++++++++++
                <-
            ]
            >
            [
                -
            ]
            <
            ,.,.
          """
        ) 
      ) 
    ) shouldBe OutputData("Co").asRight

    run(
      ProgramData("Conqueror of British Empire (CWE). All hail Idi Amin!!!",
        strip(
          """
            +++++
            [>
                ++++++++
                [>
                    ++++++++++
                    [
                        >
                        ++++++++++++
                        >
                        ++++++++++++
                        >
                        ++++++++++++
                        >
                        +++++++++++
                        >
                        +++++++++++
                        <<<<<
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    >
                    [
                        -
                    ]
                    <<<<<
                    <-
                ]
                <-
            ]

            ++++++++++++++
            [
                >
                +++++++++++++++
                <-
            ]
            >
            [
                -
            ]
            <
            ,.,+.
          """
        ) 
      ) 
    ) shouldBe ErrorMessage("PROCESS TIME OUT. KILLED!!!", OutputData("C").some).asLeft
  }
}
