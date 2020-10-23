package brainfk

import io.StdIn._
import scala.language.postfixOps
import scala.sys.process.processInternal
import scala.util.chaining._
import scala.annotation.tailrec

import cats._
import cats.implicits._
import scala.util.chaining._

object Main {
  case class ProgramData (input: String, code: String)
  case class CompiledProgram (input: String, code: Vector[Command])

  case class ErrorMessage (message: String, output: Option[OutputData] = None)
  case class OutputData (data: String) extends AnyVal

  sealed trait Representationable {def representation: Char}
  sealed trait Amountable {def amount: Int}
  sealed trait Command extends Representationable with Amountable
  sealed trait CommandObject extends Representationable 
  sealed trait BracketCommand {def jumpTo: Int}
  object Command {

    case class IncMemPtr(amount: Int) extends Command {
      val representation = IncMemPtr.representation
    }
    object IncMemPtr extends CommandObject {
      val representation = '>'
    }

    case class DecMemPtr(amount: Int) extends Command {
      val representation = DecMemPtr.representation
    }
    object DecMemPtr extends CommandObject {
      val representation = '<'
    }

    case class IncCell(amount: Int) extends Command {
      val representation = IncCell.representation
    }
    object IncCell extends CommandObject {
      val representation = '+'
    }

    case class DecCell(amount: Int) extends Command {
      val representation = DecCell.representation
    }
    object DecCell extends CommandObject {
      val representation = '-'
    }

    case class OutChar(amount: Int) extends Command {
      val representation = OutChar.representation
    }
    object OutChar extends CommandObject {
      val representation = '.'
    }

    case class InChar(amount: Int) extends Command {
      val representation = InChar.representation
    }
    object InChar extends CommandObject {
      val representation = ','
    }

    case class OpenBracket(amount: Int, jumpTo: Int) extends Command with BracketCommand  {
      val representation = OpenBracket.representation
    }
    object OpenBracket extends CommandObject {
      val representation = '['
    }

    case class CloseBracket(amount: Int, jumpTo: Int) extends Command with BracketCommand  {
      val representation = CloseBracket.representation
    }
    object CloseBracket extends CommandObject {
      val representation = ']'
    }
  }

  object Compiler {
    import Command._

    private def find(str: String, c: Char): Option[Int] = str indexOf c match {
      case -1     => None
      case index  => Option(index)
    }
    def findClose(i: Int, code: String): Option[Int] = find(code drop i + 1, ']') match {
      case None => None
      case Some(v) => Some(i + v + 1)
    }
    def findOpen(i: Int, code: String): Option[Int] = find((code take i) reverse, '[') match {
      case None => None
      case Some(v) => Some(i - v - 1)
    }

    def getCommandObject (c: Char) = c match {
      case IncMemPtr.representation     => IncMemPtr
      case DecMemPtr.representation     => DecMemPtr
      case IncCell.representation       => IncCell
      case DecCell.representation       => DecCell
      case InChar.representation        => InChar
      case OutChar.representation       => OutChar
      case OpenBracket.representation   => OpenBracket
      case CloseBracket.representation  => CloseBracket
    }

    def getCommand (c: CommandObject, amount: Int, jumpTo: Int = -1) = c match {
      case IncMemPtr    => IncMemPtr(amount)
      case DecMemPtr    => DecMemPtr(amount)
      case IncCell      => IncCell(amount)
      case DecCell      => DecCell(amount)
      case OutChar      => OutChar(amount)
      case InChar       => InChar(amount)
      case OpenBracket  => OpenBracket(amount, jumpTo)
      case CloseBracket => CloseBracket(amount, jumpTo)
    }

    def compile (program: ProgramData): CompiledProgram = {

      @tailrec
      def compileRecursive (
        compiled: Vector[Command],
        i: Int,
        prevCommandObj: CommandObject,
        commandCount: Int
      ): Vector[Command] = {
        if (i != program.code.length) {
          val c = program.code(i)
          val currCommandObj = getCommandObject(c)
          if (prevCommandObj.representation == c)
            compileRecursive(compiled, i + 1, currCommandObj, commandCount + 1)
          else {
            val newCompiled = compiled :+ getCommand(prevCommandObj, commandCount + 1)
            compileRecursive(newCompiled, i + 1, currCommandObj, 0)
          }
        } else compiled :+ getCommand(prevCommandObj, commandCount + 1)
      }

      def setBracketsValue (compiled: Vector[Command]): Vector[Command] = {
        val helperString = compiled.map(_.representation).mkString
        compiled.zipWithIndex.map {
          case (OpenBracket(amount, _), i)  => OpenBracket(amount, findClose(i, helperString).get)
          case (CloseBracket(amount, _), i)  => CloseBracket(amount, findOpen(i, helperString).get)
          case v @ _ => v._1
        }
      }

      val firstCommand = getCommandObject(program.code.head)
      val compiled = compileRecursive(Vector[Command](), 1, firstCommand, 0)
      val withBracketVals = setBracketsValue(compiled)
      CompiledProgram(program.input, withBracketVals)
    }
  }

  object Executor {
    import Command._

    def runPg (program: CompiledProgram): Either[
      ErrorMessage,
      OutputData
    ] = {
      val memSize = 100_000
      val watchdogLimit = 100_000

      def createMemoryChunk(size: Int) =
        Vector.fill[Char](size)(0)

      @tailrec
      def runRecursive (
        memory: Vector[Char] = Vector[Char](),
        output: String, 
        progPtr: Int,
        memPtr: Int,
        inputPtr: Int,
        watchdogCounter: Int
      ): Either[ErrorMessage, OutputData] = {
        if (memPtr < 0) {
          ErrorMessage(
            s"Segmentation fault! Memory pointer was at $memPtr",
            OutputData(output).some
          ).asLeft
        } else if (watchdogCounter >= watchdogLimit) {
          ErrorMessage(
            "PROCESS TIME OUT. KILLED!!!",
            OutputData(output).some
          ).asLeft
        } else if (progPtr >= program.code.length) {
          OutputData(output).asRight
        } else if (memPtr > memory.length) {
          runRecursive(
            memory ++ createMemoryChunk(memSize / 2),
            output,
            progPtr,
            memPtr,
            inputPtr,
            watchdogCounter + 1
          )
        } else program.code(progPtr) match {
          case IncMemPtr(amount) => 
            runRecursive(
              memory, 
              output, 
              progPtr + 1, 
              memPtr + amount, 
              inputPtr, 
              watchdogCounter + 1
            )
          case DecMemPtr(amount) => 
            runRecursive(
              memory, 
              output, 
              progPtr + 1, 
              memPtr - amount, 
              inputPtr, 
              watchdogCounter + 1
            )
          case IncCell(amount) => 
            runRecursive(
              memory.updated(memPtr, (memory(memPtr) + amount).toChar), 
              output, 
              progPtr + 1, 
              memPtr, 
              inputPtr,
              watchdogCounter + 1
            ) 
          case DecCell(amount) =>
            runRecursive(
              memory.updated(memPtr, (memory(memPtr) - amount).toChar), 
              output, 
              progPtr + 1, 
              memPtr, 
              inputPtr,
              watchdogCounter + 1
            ) 
          case OutChar(amount) =>
            runRecursive(
              memory,
              output + memory(memPtr).toString * amount,
              progPtr + 1,
              memPtr,
              inputPtr,
              watchdogCounter + 1
            ) 
          case InChar(amount) => 
            runRecursive( // TODO: fix this thing
              memory.updated(memPtr, program.input(inputPtr)),
              output,
              progPtr + 1,
              memPtr,
              inputPtr + 1,
              watchdogCounter + 1
            )
          case OpenBracket(amount, jumpTo) => 
            runRecursive( 
              memory,
              output,
              if (memory(memPtr) == 0) jumpTo else progPtr + 1,
              memPtr,
              inputPtr, 
              watchdogCounter + 1
            )
          case CloseBracket(amount, jumpTo) => 
            runRecursive( 
              memory,
              output,
              if (memory(memPtr) != 0) jumpTo else progPtr + 1,
              memPtr,
              inputPtr,
              watchdogCounter + 1
            )
        }
      }

      runRecursive(createMemoryChunk(memSize), "", 0, 0, 0, 0)
    } 
  }

  object Renderer {
    def renderResults[A](output: Either[ErrorMessage, OutputData]): String =
      output match {
        case Right(out) => out.data
        case Left(err) => 
          s"${err.output.getOrElse(OutputData("")).data}\n${err.message}"
      }
  }

  object ProgramChecker {
    import Compiler._

    def checkBrackets (program: ProgramData): Either[ErrorMessage, ProgramData] = {
      // find last '[' bracket and first ']' bracket...
      // using out of bounds indexes because normally those finders ignore bracket at index
      val lastOpen   = findOpen(program.code.length, program.code).getOrElse(program.code.length)
      val firstClose = findClose(-1, program.code).getOrElse(-1)

      // ...and find them a pair. If there is no pair for any of them, return a Left
      // if there are a pairs, return a Right
      (
        findClose(lastOpen, program.code), 
        findOpen(firstClose, program.code)
      ) match {        // check if it was found at all
        case (None, _) if(lastOpen   != program.code.length) 
          => Left(ErrorMessage(s"Found excess '[' at $lastOpen"))
        case (_, None) if(firstClose != -1)                   
          => Left(ErrorMessage(s"Found excess ']' at $firstClose"))
        case _ => Right(program)
      }
    }
  }

  def main (args: Array[String]) = {
    val N = (readLine split " " last).toIntOption getOrElse(0)
    val input = readLine

    val code = (1 to N) map (_ => readLine) mkString "" filter (c => "<>+-.,[]" contains c)
    
    val program = ProgramData(input, code)

    val output = for {
      _         <- ProgramChecker.checkBrackets(program)
      compiled  <- Compiler.compile(program).asRight
      results   <- Executor.runPg(compiled)
    } yield results

    val rendered = Renderer.renderResults(output)

    println(rendered)
  }
}