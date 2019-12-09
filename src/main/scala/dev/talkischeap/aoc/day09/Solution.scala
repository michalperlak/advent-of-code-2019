package dev.talkischeap.aoc.day09

import scala.io.Source

object Solution {
  sealed trait ExecMode
  case object Running extends ExecMode
  case object Terminated extends ExecMode
  case object Halted extends ExecMode

  case class ExecState(program: Map[BigInt, BigInt], pointer: BigInt = 0, input: List[BigInt],
                       output: List[BigInt] = Nil, baseOffset: BigInt = 0)

  def partOneSolution(): BigInt = executeWithInput(List(1))

  def partTwoSolution(): BigInt = executeWithInput(List(2))

  def executeWithInput(input: List[BigInt]): BigInt = {
    val program = readInput()
    execute(ExecState(program, input = input))
      ._2
      .output
      .head
  }

  @scala.annotation.tailrec
  def execute(state: ExecState): (ExecMode, ExecState) = {
    val (mode, next) = executeNext(state)
    if (mode == Running) execute(next)
    else (mode, next)
  }

  private def executeNext(currentState: ExecState): (ExecMode, ExecState) = {
    def offset(i: BigInt): BigInt = currentState.pointer + i

    val program = currentState.program
    def immediate(i: BigInt): BigInt = program(offset(i))
    def position(i: BigInt): BigInt = program(immediate(i))
    def relative(i: BigInt): BigInt = program(immediate(i) + currentState.baseOffset)

    val (opcode, modes) = {
      val code = immediate(0).toString.reverse.padTo(5, '0')
      val (op, params) = code.splitAt(2)
      (op.reverse.toInt, params.toSeq.map(_.asDigit))
    }

    def parameter(index: Int): BigInt = modes(index - 1) match {
      case 0 => position(index)
      case 1 => immediate(index)
      case 2 => relative(index)
    }

    def address(index: Int): BigInt = modes(index - 1) match {
      case 0 => immediate(index)
      case 2 => immediate(index) + currentState.baseOffset
    }

    def toInt(value: Boolean): BigInt = if (value) 1 else 0

    def binaryOp(operation: (BigInt, BigInt) => BigInt): ExecState =
      currentState.copy(
        program = program + (address(3) -> operation(parameter(1), parameter(2))),
        pointer = offset(4)
      )

    def jumpIf(condition: BigInt => Boolean): ExecState =
      currentState.copy(
        pointer = if (condition(parameter(1))) parameter(2) else offset(3)
      )

    def storeIf(condition: (BigInt, BigInt) => Boolean): ExecState =
      currentState.copy(
        program = program + (address(3) -> toInt(condition(parameter(1), parameter(2)))),
        pointer = offset(4)
      )

    opcode match {
      case 1 => (Running, binaryOp(_ + _))
      case 2 => (Running, binaryOp(_ * _))
      case 3 =>
        currentState.input match {
          case head :: tail => (Running, currentState.copy(
            program = program + (address(1) -> head),
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
      case 9 => (Running, currentState.copy(baseOffset = currentState.baseOffset + parameter(1), pointer = offset(2)))
      case 99 => (Terminated, currentState)
    }
  }

  private def readInput(): Map[BigInt, BigInt] = parse(Source
    .fromResource("day09/input.txt")
    .getLines
    .mkString)

  private def parse(line: String): Map[BigInt, BigInt] = line
    .split(',')
    .map(BigInt.apply)
    .zipWithIndex
    .map(_.swap)
    .map(value => value.copy(_1 = BigInt(value._1)))
    .toMap
    .withDefaultValue(BigInt(0))
}
