package dev.talkischeap.aoc.day08

import scala.io.Source

object Solution {
  type Layer = List[Int]

  def partOneSolution(): Int = {
    val width = 25
    val height = 6
    val layers = readInput(width, height)
    def countN(n: Int, layer: Layer): Int = layer.count(_ == n)
    def countZeros(layer: Layer): Int = countN(0, layer)
    val layer = layers.minBy(countZeros)
    countN(1, layer) * countN(2, layer)
  }

  def partTwoSolution(): Layer = {
    val width = 25
    val height = 6
    val layers = readInput(width, height)
    @scala.annotation.tailrec
    def composeLayers(layers: List[Layer], index: Int = 0, layer: Layer = Nil): Layer = {
      if (index >= layers.head.size) layer.reverse
      else {
        val visibleLayer = layers.find(layer => layer(index) != 2).get
        composeLayers(layers, index + 1, visibleLayer(index) :: layer)
      }
    }
    composeLayers(layers)
  }

  private def readInput(width: Int, height: Int): List[Layer] = Source
    .fromResource("day08/input.txt")
    .getLines
    .flatMap(line => parseLayers(line, width * height))
    .toList

  private def parseLayers(line: String, layerLength: Int): List[Layer] = line
    .trim
    .toCharArray
    .map(_ - '0')
    .toList
    .grouped(layerLength)
    .toList
}
