package dev.talkischeap.aoc.day04

object Solution {

  def solution(): Int = {
    val min = 137683
    val max = 596253
    (min to max)
      .iterator
      .count(matchesCriteria)
  }

  private def matchesCriteria(value: Int): Boolean = {
    @scala.annotation.tailrec
    def checkDigit(digit: Char, prevDigit: Option[Char], adjacent: Set[Char], largerGroups: Set[Char], digits: List[Char]): Boolean = {
      val decreasing = prevDigit.exists(prev => digit < prev)
      if (decreasing)
        false
      else if (digits.isEmpty)
        (updateAdjacent(adjacent, digit, prevDigit) -- updateLargerGroups(largerGroups, adjacent, digit, prevDigit)).nonEmpty
      else
        checkDigit(digits.head, Option(digit), updateAdjacent(adjacent, digit, prevDigit),
          updateLargerGroups(largerGroups, adjacent, digit, prevDigit), digits.tail)
    }

    def updateAdjacent(current: Set[Char], digit: Char, prevDigit: Option[Char]): Set[Char] =
      if (areAdjacentDigits(prevDigit, digit)) current + digit
      else current
    def updateLargerGroups(current: Set[Char], adjacent: Set[Char], digit: Char, prevDigit: Option[Char]): Set[Char] =
      if (adjacent.contains(digit) && areAdjacentDigits(prevDigit, digit)) current + digit
      else current
    def areAdjacentDigits(prev: Option[Char], current: Char): Boolean = prev.contains(current)


    val digits = value.toString.toList
    checkDigit(digits.head, Option.empty, Set(), Set(), digits.tail)
  }
}
