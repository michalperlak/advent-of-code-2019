package dev.talkischeap.aoc.day05

import scala.io.{Source, StdIn}

object Solution {
  def partOneSolution(): Int = {
    val input = readInput()
    executeProgram(input)
    input(0)
  }

  private def executeProgram(input: Array[Int]): Unit = {
    @scala.annotation.tailrec
    def loop(currentIndex: Int, output: List[Int]): Unit = {
      val opcode = input(currentIndex)
      if (opcode == 99) return
      val (opPointer, newOutput) = executeOp(opcode, input, currentIndex + 1, output)
      loop(opPointer, newOutput)
    }

    loop(0, List())
  }

  private def executeOp(opcode: Int, input: Array[Int], startIndex: Int, output: List[Int]): (Int, List[Int]) = {
    val digits = opcode.toString.toCharArray
    val operation = digits.last - '0'
    if (operation == 1) (executeAdd(input, digits, startIndex), output)
    else if (operation == 2) (executeMultiply(input, digits, startIndex), output)
    else if (operation == 3) (startIndex + 1, output :+ getInput)
    else if (operation == 4) (outputValue(output.head, startIndex), output.tail)
    else throw new IllegalStateException()
  }

  private def executeMultiply(input: Array[Int], opcode: Array[Char], parameterIndex: Int): Int =
    executeBinaryOp(input, opcode, parameterIndex, (a, b) => a * b)

  private def executeAdd(input: Array[Int], opcode: Array[Char], parameterIndex: Int): Int =
    executeBinaryOp(input, opcode, parameterIndex, (a, b) => a + b)

  private def executeBinaryOp(input: Array[Int], opcode: Array[Char],
                              parameterIndex: Int, op: (Int, Int) => Int): Int = {
    val parameterModes = opcode.dropRight(2)
    val fullParameterModes =
      if (parameterModes.length < 3) Array.fill(3 - parameterModes.length)(0) ++ parameterModes.map(digit => digit - '0')
      else parameterModes.map(digit => digit - '0')

    val left = getParameter(input(parameterIndex), input, fullParameterModes(0))
    val right = getParameter(input(parameterIndex + 1), input, fullParameterModes(1))
    val position = getParameter(parameterIndex + 2, input, fullParameterModes(2))
    input(position) = op(left, right)
    parameterIndex + 3
  }

  private def getParameter(index: Int, input: Array[Int], mode: Int): Int = {
    if (mode == 0) input(index)
    else index
  }

  private def getInput: Int = StdIn.readInt

  private def outputValue(value: Int, startIndex: Int): Int = {
    println(value)
    startIndex + 1
  }

  private def readInput(): Array[Int] = Source
    .fromResource("day05/input.txt")
    .getLines
    .flatMap(parseLine)
    .toArray

  private def parseLine(line: String): Iterator[Int] = line
    .split(',')
    .map(_.toInt)
    .iterator
}
