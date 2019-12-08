package dev.talkischeap.aoc.day06

import scala.io.Source

object Solution {

  def solutionPartOne(): Int = {
    val input = readInput()
    val nodes = input.flatMap{ case (a, b) => Seq(a, b) }.toSet
    val orbitMap = buildOrbitMap(input, Map())
    nodes
      .toSeq
      .map(node => countOrbits(node, orbitMap))
      .sum
  }

  def solutionPartTwo(): Int = {
    val input = readInput()
    val orbits = buildOrbitMap(input ++ input.map(_.swap), Map())
    bfs("SAN", orbits, Set("YOU"))
  }

  private def countOrbits(objectName: String, orbits: Map[String, Set[String]]): Int =
    orbits
      .get(objectName)
      .toSeq
      .flatten
      .map(to => 1 + countOrbits(to, orbits))
      .sum

  @scala.annotation.tailrec
  private def buildOrbitMap(input: List[(String, String)], output: Map[String, Set[String]]): Map[String, Set[String]] = {
    if (input.isEmpty) output
    else {
      val next = input.head
      val orbits = output.getOrElse(next._1, Set())
      buildOrbitMap(input.tail, output.updated(next._1, orbits + next._2))
    }
  }

  @scala.annotation.tailrec
  def bfs(dest: String, graph: Map[String, Set[String]], current: Set[String],
          visited: Set[String] = Set.empty, hops: Int = -2): Int = {
    if (current.contains(dest)) {
      hops
    } else {
      val nextVisited = current ++ visited
      val next = current.flatMap(graph.get).flatten.diff(nextVisited)
      bfs(dest, graph, next, nextVisited, hops + 1)
    }
  }

  private def readInput(): List[(String, String)] = Source
    .fromResource("day06/input.txt")
    .getLines
    .map(parseLine)
    .toList

  private def parseLine(line: String): (String, String) = {
    val parts = line.split(')')
    (parts(0), parts(1))
  }
}
