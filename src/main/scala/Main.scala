package brainfk

import io.StdIn._
import scala.language.postfixOps
import scala.sys.process.processInternal
import scala.util.chaining._

object Main {
  case class ProgramData (input: String, code: String)
  case class ErrorMessage (message: String)
  case class OutputData (data: String)

  def parseProgram(program: ProgramData): Either[ErrorMessage, OutputData] = {
    //def looper
    ???
  } 

  object Executor {
    private def find(str: String, c: Char): Option[Int] = str indexOf c match {
      case -1     => None
      case index  => Option(index)
    }
    def findClose(i: Int, program: ProgramData): Option[Int] = find(program.code drop i + 1, ']') match {
      case None => None
      case Some(v) => Some(i + v + 1)
    }
    def findOpen(i: Int, program: ProgramData): Option[Int] = find((program.code take i) reverse, '[') match {
      case None => None
      case Some(v) => Some(i - v - 1)
    }
  }

  object ProgramChecker {
    import Executor._

    def checkBrackets (program: ProgramData): Either[ErrorMessage, ProgramData] = {
      val lastOpen   = findOpen(program.code.length, program).getOrElse(program.code.length)
      val firstClose = findClose(0, program).getOrElse(0)

      (
        findClose(lastOpen, program), 
        findOpen(firstClose, program)
      ) match {
        case (None, _) if(lastOpen   != program.code.length) 
          => Left(ErrorMessage(s"Found excess '[' at $lastOpen"))
        case (_, None) if(firstClose != 0)                   
          => Left(ErrorMessage(s"Found excess ']' at $firstClose"))
        case _ => Right(program)
      }
    }
      
    def checkSegFault (program: ProgramData): Either[ErrorMessage, ProgramData] = ???

    def checkProgram(program: ProgramData): Either[ErrorMessage, ProgramData] = 
      for {
        _ <- checkBrackets(program)
        _ <- checkSegFault(program)
      } yield program
  }
  

  def main (args: Array[String]) = {
    println("started") 

    val N = (readLine split " " last).toIntOption getOrElse(0)
    val input = readLine

    val code = (1 to N) map (_ => readLine) mkString "" filter (c => "<>+-.,[]" contains c)
    
    val program = ProgramData(input, code)

    println(program)

    /*(for {
      _       <- ProgramChecker.checkProgram(program)
      output  <- parseProgram(program)
    } yield output) foreach println*/
  }
}