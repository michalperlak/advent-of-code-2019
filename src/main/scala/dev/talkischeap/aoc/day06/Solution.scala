package dev.talkischeap.aoc.day06

import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(Solution.solutionPartOne())
  }

  def solutionPartOne(): Int = {
    val input = readInput()
    val orbitMap = buildOrbitMap(input, Map())

    def countOrbits(orbitMap: Map[String, Set[String]], name: String): Int = {
      orbitMap
        .get(name)
        .map(orbits => orbits.map(orbit => countOrbits(orbitMap, orbit)).sum + orbits.size)
        .getOrElse(0)
    }

    orbitMap.map(elem => countOrbits(orbitMap, elem._1)).sum
  }

  @scala.annotation.tailrec
  private def buildOrbitMap(input: Iterator[(String, String)], output: Map[String, Set[String]]): Map[String, Set[String]] = {
    if (!input.hasNext) output
    else {
      val next = input.next
      val orbits = output.getOrElse(next._1, Set())
      buildOrbitMap(input, output.updated(next._1, orbits + next._2))
    }
  }

  private def readInput(): Iterator[(String, String)] = Source
    .fromResource("day06/input.txt")
    .getLines
    .map(parseLine)
    .iterator

  private def parseLine(line: String): (String, String) = {
    val parts = line.split(')')
    (parts(0), parts(1))
  }
}
