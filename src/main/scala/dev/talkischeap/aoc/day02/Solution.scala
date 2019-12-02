package dev.talkischeap.aoc.day02

import scala.io.Source

object Solution {
  def partOneSolution(): Int = {
    val input = readInput()
    executeProgram(input, 12, 2)
    input(0)
  }

  def partTwoSolution(): Int = {
    val input = readInput()
    val searched = 19690720
    val (noun, verb) = (0 to 99)
      .iterator
      .flatMap(noun => (0 to 99).iterator.map(verb => (noun, verb)))
      .find(value => {
        val arr = input.clone
        executeProgram(arr, value._1, value._2)
        arr(0) == searched
      }).getOrElse((0, 0))
    100 * noun + verb
  }

  private def executeProgram(input: Array[Int], noun: Int, verb: Int): Unit = {
    @scala.annotation.tailrec
    def loop(currentIndex: Int): Unit = {
      val opcode = input(currentIndex)
      if (opcode == 99) return
      val left = input(input(currentIndex + 1))
      val right = input(input(currentIndex + 2))
      val position = input(currentIndex + 3)
      val result =
        if (opcode == 1) left + right
        else if (opcode == 2) left * right
        else throw new IllegalStateException()
      input(position) = result
      loop(currentIndex + 4)
    }

    input(1) = noun
    input(2) = verb
    loop(0)
  }

  private def readInput(): Array[Int] = Source
    .fromResource("day02/input.txt")
    .getLines
    .flatMap(parseLine)
    .toArray

  private def parseLine(line: String): Iterator[Int] = line
    .split(',')
    .map(_.toInt)
    .iterator
}
