package dev.talkischeap.aoc.day10

import scala.io.Source

object Solution {

  case class Point(x: Int, y: Int) {
    def -(other: Point): Point = Point(x - other.x, y - other.y)
    def dist(other: Point): (Int, Int) = {
      val diff = other - this
      val div = gcd(Math.abs(diff.x), Math.abs(diff.y))
      (diff.x / div, diff.y / div)
    }
    @scala.annotation.tailrec
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  }

  def main(args: Array[String]): Unit = {
    println(Solution.partOneSolution())
  }

  def partOneSolution(): Int = {
    val asteroids = readInput()
    asteroids
      .map(a => a -> asteroids.filter(_ != a).map(a.dist).distinct.size)
      .maxBy(_._2)
      ._2
  }

  private def readInput(): List[Point] = {
    val positions = Source
      .fromResource("day10/input.txt")
      .getLines
      .map(_.toSeq.map(_ == '#'))
    val asteroids = for {
      (a, y) <- positions.zipWithIndex
      (b, x) <- a.zipWithIndex
      if b
    } yield Point(x, y)
    asteroids.toList
  }
}
