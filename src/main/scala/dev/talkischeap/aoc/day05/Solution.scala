package dev.talkischeap.aoc.day05

import scala.io.{Source, StdIn}

object Solution {
  def solution(): Unit = {
    val input = readInput()
    executeProgram(input)
  }

  private def executeProgram(input: Array[Int]): Unit = {
    @scala.annotation.tailrec
    def loop(currentIndex: Int): Unit = {
      val opcode = input(currentIndex)
      if (opcode == 99) return
      val opPointer = executeOp(opcode, input, currentIndex + 1)
      loop(opPointer)
    }

    loop(0)
  }

  private def executeOp(opcode: Int, input: Array[Int], startIndex: Int): Int = {
    val digits = opcode.toString.toCharArray
    val operation = digits.last - '0'
    if (operation == 1) executeAdd(input, digits, startIndex)
    else if (operation == 2) executeMultiply(input, digits, startIndex)
    else if (operation == 3) executeGetInput(input, startIndex, digits)
    else if (operation == 4) executeOutput(input, startIndex, digits)
    else if (operation == 5) executeJumpIfTrue(input, startIndex, digits)
    else if (operation == 6) executeJumpIfFalse(input, startIndex, digits)
    else if (operation == 7) executeLessThan(input, startIndex, digits)
    else if (operation == 8) executeEquals(input, startIndex, digits)
    else throw new IllegalStateException()
  }

  private def executeMultiply(input: Array[Int], opcode: Array[Char], parameterIndex: Int): Int =
    executeBinaryOp(input, opcode, parameterIndex, (a, b) => a * b)

  private def executeAdd(input: Array[Int], opcode: Array[Char], parameterIndex: Int): Int =
    executeBinaryOp(input, opcode, parameterIndex, (a, b) => a + b)

  private def executeBinaryOp(input: Array[Int], opcode: Array[Char],
                              parameterIndex: Int, op: (Int, Int) => Int): Int = {
    val parameterModes = getParameterModes(opcode, 3)
    val left = getParameter(parameterIndex, input, parameterModes(2))
    val right = getParameter(parameterIndex + 1, input, parameterModes(1))
    val position = getParameter(parameterIndex + 2, input, parameterModes(0))
    input(position) = op(left, right)
    parameterIndex + 3
  }

  private def executeGetInput(input: Array[Int], parameterIndex: Int, opcode: Array[Char]): Int = {
    val inputValue = StdIn.readInt
    val parameter = getParameter(parameterIndex, input, 1)
    input(parameter) = inputValue
    parameterIndex + 1
  }

  private def executeOutput(input: Array[Int], parameterIndex: Int, opcode: Array[Char]): Int = {
    val parameter = getParameter(parameterIndex, input, 1)
    println(parameter)
    parameterIndex + 1
  }

  private def executeJumpIfTrue(input: Array[Int], parameterIndex: Int, opcode: Array[Char]): Int =
    executeJump(input, parameterIndex, opcode, a => a != 0)

  private def executeJumpIfFalse(input: Array[Int], parameterIndex: Int, opcode: Array[Char]): Int =
    executeJump(input, parameterIndex, opcode, a => a == 0)

  private def executeJump(input: Array[Int], parameterIndex: Int, opcode: Array[Char], cond: (Int) => Boolean): Int = {
    val parameterModes = getParameterModes(opcode, 2)
    val first = getParameter(parameterIndex, input, parameterModes(1))
    val second = getParameter(parameterIndex + 1, input, parameterModes(0))
    if (cond(first)) second else parameterIndex + 2
  }

  private def executeLessThan(input: Array[Int], parameterIndex: Int, opcode: Array[Char]): Int =
    executeCondStore(input, parameterIndex, opcode, (a,b) => a < b)

  private def executeEquals(input: Array[Int], parameterIndex: Int, opcode: Array[Char]): Int =
    executeCondStore(input, parameterIndex, opcode, (a,b) => a == b)

  private def executeCondStore(input: Array[Int], parameterIndex: Int, opcode: Array[Char], cond: (Int, Int) => Boolean): Int = {
    val parameterModes = getParameterModes(opcode, 3)
    val first = getParameter(parameterIndex, input, parameterModes(2))
    val second = getParameter(parameterIndex + 1, input, parameterModes(1))
    val third = getParameter(parameterIndex + 2, input, parameterModes(0))
    input(third) = if (cond(first, second)) 1 else 0
    parameterIndex + 3
  }

  private def getParameterModes(opcode: Array[Char], parameters: Int): Array[Int] = {
    val parameterModes = opcode.dropRight(2)
    if (parameterModes.length < parameters)
      Array.fill(parameters - parameterModes.length)(0) ++ parameterModes.map(digit => digit - '0')
    else
      parameterModes.map(digit => digit - '0')
  }

  private def getParameter(index: Int, input: Array[Int], mode: Int): Int = {
    if (mode == 0) input(input(index))
    else input(index)
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
