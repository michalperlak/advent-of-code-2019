package dev.talkischeap.aoc.day07

import scala.collection.immutable.ArraySeq
import scala.io.Source

object Solution {
  sealed trait ExecMode
  case object Running extends ExecMode
  case object Terminated extends ExecMode
  case object Halted extends ExecMode

  case class ExecState(program: IndexedSeq[Int], pointer: Int = 0, input: List[Int], output: List[Int] = Nil)

  def main(args: Array[String]): Unit = {
    println(Solution.partOneSolution())
  }

  def partOneSolution(): Int = {
    val program = readInput()
    def chained(phases: Seq[Int]): Int =
      phases.foldLeft(0)((a, e) => execute(ExecState(program, input = List(e, a)))._2.output.head)

    (0 to 4)
      .permutations
      .map(chained)
      .max
  }

  @scala.annotation.tailrec
  def execute(state: ExecState): (ExecMode, ExecState) = {
    val (mode, next) = executeNext(state)
    if (mode == Running) execute(next)
    else (mode, next)
  }

  private def executeNext(currentState: ExecState): (ExecMode, ExecState) = {
    def offset(i: Int): Int = currentState.pointer + i

    val program = currentState.program
    def immediate(i: Int): Int = program(offset(i))
    def position(i: Int): Int = program(immediate(i))

    val (opcode, modes) = {
      val code = immediate(0).toString.reverse.padTo(5, '0')
      val (op, params) = code.splitAt(2)
      (op.reverse.toInt, params.toSeq.map(_ != '0'))
    }

    def parameter(index: Int): Int = if (modes(index - 1)) immediate(index) else position(index)

    def toInt(value: Boolean): Int = if (value) 1 else 0

    def binaryOp(operation: (Int, Int) => Int): ExecState =
      currentState.copy(
        program = program.updated(immediate(3), operation(parameter(1), parameter(2))),
        pointer = offset(4)
      )

    def jumpIf(condition: Int => Boolean): ExecState =
      currentState.copy(
        pointer = if (condition(parameter(1))) parameter(2) else offset(3)
      )

    def storeIf(condition: (Int, Int) => Boolean): ExecState =
      currentState.copy(
        program = program.updated(immediate(3), toInt(condition(parameter(1), parameter(2)))),
        pointer = offset(4)
      )

    opcode match {
      case 1 => (Running, binaryOp(_ + _))
      case 2 => (Running, binaryOp(_ * _))
      case 3 =>
        currentState.input match {
          case head :: tail => (Running, currentState.copy(
            program = program.updated(immediate(1), head),
            pointer = offset(2),
            input = tail
          ))
          case Nil => (Halted, currentState)
        }
      case 4 => (Running, currentState.copy(pointer = offset(2), output = currentState.output :+ parameter(1)))
      case 5 => (Running, jumpIf(_ != 0))
      case 6 => (Running, jumpIf(_ == 0))
      case 7 => (Running, storeIf(_ < _))
      case 8 => (Running, storeIf(_ == _))
      case 99 => (Terminated, currentState)
    }
  }

  private def readInput(): IndexedSeq[Int] = Source
    .fromResource("day07/input.txt")
    .getLines
    .flatMap(parseLine)
    .toIndexedSeq

  private def parseLine(line: String): Iterator[Int] = line
    .split(',')
    .map(_.toInt)
    .iterator
}
