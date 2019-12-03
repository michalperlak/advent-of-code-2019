package dev.talkischeap.aoc.day03

import scala.io.Source

object Solution {

  case class Move(direction: Direction, distance: Distance) {
    def points(x: Int, y: Int): (Int, Int, Set[Point]) = {
      direction match {
        case Up => (x, y + distance, (y + 1 to y + distance).map(newY => (x, newY)).toSet)
        case Down => (x, y - distance, (y - distance until y).map(newY => (x, newY)).toSet)
        case Left => (x - distance, y, (x - distance until x).map(newX => (newX, y)).toSet)
        case Right => (x + distance, y, (x + 1 to x + distance).map(newX => (newX, y)).toSet)
      }
    }
  }

  type Distance = Int
  type Wire = List[Move]
  type Point = (Int, Int)


  def partOneSolution(): Distance = {
    val input = readInput()
    val wire1 = input.next
    val wire2 = input.next
    minManhattanDistance(wire1, wire2)
  }

  def partTwoSolution(): Int = {
    val input = readInput()
    val wire1 = input.next
    val wire2 = input.next
    minStepsIntersection(wire1, wire2)
  }

  private def minManhattanDistance(w1: Wire, w2: Wire): Distance = {
    val points1 = visitedPoints(w1)
    val points2 = visitedPoints(w2)
    val intersectionPoints = points1.intersect(points2)
    intersectionPoints
      .map(manhattanDistance)
      .min
  }

  private def minStepsIntersection(w1: Wire, w2: Wire): Int = {
    val points1 = visitedPointsWithRequiredSteps(w1)
    val points2 = visitedPointsWithRequiredSteps(w2)
    val intersectionPoints = points1.keySet.intersect(points2.keySet)
    intersectionPoints
      .map(point => points1(point) + points2(point))
      .min
  }

  private def manhattanDistance(point: Point): Distance = Math.abs(point._1) + Math.abs(point._2)

  private def visitedPoints(wire: Wire): Set[Point] = {
    @scala.annotation.tailrec
    def move(x: Int, y: Int, points: Set[Point], moves: List[Move]): Set[Point] =
      if (moves.isEmpty) {
        points
      } else {
        val nextMove = moves.head
        val (newX, newY, newPoints) = nextMove.points(x, y)
        move(newX, newY, points ++ newPoints, moves.tail)
      }

    move(0, 0, points = Set(), moves = wire)
  }

  private def visitedPointsWithRequiredSteps(wire: Wire): Map[Point, Int] = {
    @scala.annotation.tailrec
    def move(x: Int, y: Int, steps: Int, points: Map[Point, Int], moves: List[Move]): Map[Point, Int] =
      if (moves.isEmpty) {
        points
      } else {
        val nextMove = moves.head
        val (newX, newY, newPoints) = nextMove.points(x, y)
        val startPoint = (x, y)
        val newPointsWithRequiredSteps = newPoints
          .map(point => (point, steps + stepsToMove(startPoint, point)))
          .toMap
        move(newX, newY, steps + stepsToMove(startPoint, (newX, newY)), points ++ newPointsWithRequiredSteps, moves.tail)
      }

    move(0, 0, 0, points = Map(), moves = wire)
  }

  private def stepsToMove(from: Point, to: Point): Int =
    Math.abs(from._1 - to._1) + Math.abs(from._2 - to._2)

  private def readInput(): Iterator[Wire] = Source
    .fromResource("day03/input.txt")
    .getLines
    .map(parseWire)

  private def parseWire(line: String): Wire = line
    .split(',')
    .map(_.trim)
    .map(parseMove)
    .toList

  private def parseMove(move: String): Move = {
    val direction = Direction.parse(move)
    val distance = move.drop(1).toInt
    Move(direction, distance)
  }
}