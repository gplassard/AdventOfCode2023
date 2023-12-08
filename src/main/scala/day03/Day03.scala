package fr.gplassard.adventofcode
package day03

object Day03 {

  def part1(lines: List[String]): Int =
    val engineMap2D = lines.map(line => line.toCharArray.toList)
    val width = engineMap2D.headOption.map(_.length).getOrElse(0)
    val height = engineMap2D.length
    var sum = 0
    var currentPartNumber = ""
    var hasNeighbor = false
    for (row <- 0 until height) {
      for (col <- 0 until width) {
        val element = engineMap2D(row)(col)
        if (element.isDigit) {
          hasNeighbor = hasNeighbor || hasPartNeighbor(col, row, engineMap2D, width, height)
          currentPartNumber = currentPartNumber + element
        }
        else if (currentPartNumber.nonEmpty && hasNeighbor) {
          sum = sum + currentPartNumber.toInt
        }
        if (!element.isDigit) {
          currentPartNumber = ""
          hasNeighbor = false
        }
      }
    }
    sum

  def hasPartNeighbor(col: Int, row: Int, engineMap2D: List[List[Char]], width: Int, height: Int): Boolean =
    neighbors(col, row, engineMap2D, width, height).exists((col, row) => isPart(engineMap2D(row)(col)))

  def neighbors(col: Int, row: Int, engineMap2D: List[List[Char]], width: Int, height: Int): List[(Int, Int)] = List(
    (Math.max(col - 1, 0), Math.max(row - 1, 0)),
    (Math.max(col - 1, 0), Math.max(row - 0, 0)),
    (Math.max(col - 1, 0), Math.min(row + 1, height - 1)),
    (Math.max(col - 0, 0), Math.max(row - 1, 0)),
    (Math.max(col - 0, 0), Math.max(row - 0, 0)),
    (Math.max(col - 0, 0), Math.min(row + 1, height - 1)),
    (Math.min(col + 1, width - 1), Math.max(row - 1, 0)),
    (Math.min(col + 1, width - 1), Math.max(row - 0, 0)),
    (Math.min(col + 1, width - 1), Math.min(row + 1, height - 1)),
  )

  def isPart(c: Char): Boolean = c != '.' && !c.isDigit


  def part2(lines: List[String]): Int =
    val engineMap2D = lines.map(line => line.toCharArray.toList)
    val width = engineMap2D.headOption.map(_.length).getOrElse(0)
    val height = engineMap2D.length
    var gearCandidates: Map[(Int, Int), List[Int]] = Map.empty
    var currentPartNumber = ""
    var starNeighbors: Set[(Int, Int)] = Set.empty
    for (row <- 0 until height) {
      for (col <- 0 until width) {
        val element = engineMap2D(row)(col)
        if (element.isDigit) {
          starNeighbors = starNeighbors ++ getStarNeighborsPositions(col, row, engineMap2D, width, height)
          currentPartNumber = currentPartNumber + element
        }
        else if (currentPartNumber.nonEmpty && starNeighbors.nonEmpty) {
          for (neighbor <- starNeighbors) {
            val updatedGear = currentPartNumber.toInt :: gearCandidates.getOrElse(neighbor, List.empty)
            gearCandidates = gearCandidates + (neighbor -> updatedGear)
          }
        }
        if (!element.isDigit) {
          currentPartNumber = ""
          starNeighbors = Set.empty
        }
      }
    }
    gearCandidates
      .values
      .filter(_.length == 2)
      .map(_.product)
      .sum

  def getStarNeighborsPositions(col: Int, row: Int, engineMap2D: List[List[Char]], width: Int, height: Int): List[(Int, Int)] =
    neighbors(col, row, engineMap2D, width, height).filter((col, row) => engineMap2D(row)(col) == '*')

}
