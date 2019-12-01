package dev.talkischeap.aoc.day01

import scala.io.Source

object Solution {
  type Mass = Int
  type Fuel = Long

  def solution(): Fuel = readInput()
    .map(computeFuelRequirement)
    .sum

  private def computeFuelRequirement(mass: Mass): Fuel = {
    @scala.annotation.tailrec
    def compute(totalRequirement: Fuel, currentMass: Mass): Fuel = {
      val currentRequirement = Math.max(0, currentMass / 3 - 2)
      if (currentRequirement > 0) compute(totalRequirement + currentRequirement, currentRequirement)
      else totalRequirement
    }

    compute(0L, mass)
  }

  private def readInput(): Iterator[Mass] = Source
    .fromResource("day01/input.txt")
    .getLines
    .map(_.toInt)
}
