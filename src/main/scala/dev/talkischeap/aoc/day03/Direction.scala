package dev.talkischeap.aoc.day03

sealed class Direction

case object Up extends Direction

case object Down extends Direction

case object Left extends Direction

case object Right extends Direction

object Direction {
  def parse(move: String): Direction = move.take(1) match {
    case "U" => Up
    case "D" => Down
    case "L" => Left
    case "R" => Right
  }
}
