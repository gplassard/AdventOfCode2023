package fr.gplassard.adventofcode
package day21

import scala.collection.mutable.{Set => MSet, Queue => MQueue}

object Day21 {
  def part1(lines: List[String]): Int = {
    val grid = lines.map(_.toCharArray.toList)
    val start = lines.zipWithIndex
      .find((l, row) => l.indexOf("S") >= 0)
      .map((l, row) => (row, l.indexOf("S")))
      .get
    solve(start, grid, 64)
  }

  def findNeighbors(
      point: (Int, Int),
      grid: List[List[Char]]
  ): List[(Int, Int)] =
    List((0, 1), (0, -1), (1, 0), (-1, 0))
      .map(delta => (point._1 + delta._1, point._2 + delta._2))
      .filter(p =>
        p._1 >= 0 && p._1 < grid.length && p._2 >= 0 && p._2 < grid.head.length
      )
      .filter(p => grid(p._1)(p._2) != '#')

  def solve(start: (Int, Int), grid: List[List[Char]], depth: Int): Int =
    val all = MSet.empty[(Int, Int)]
    val visited = MSet(start)
    val todos = MQueue((start, depth))
    while (todos.nonEmpty) {
      val (point, remaining) = todos.dequeue()
      if (remaining % 2 == 0) {
        all += point
      }
      if (remaining > 0) {
        val neighbors = findNeighbors(point, grid).filterNot(visited.contains)
        for (neighbor <- neighbors) {
          visited += neighbor
          todos.enqueue((neighbor, remaining - 1))
        }
      }
    }
    all.size

  def part2(lines: List[String]): Long = {
    val grid = lines.map(_.toCharArray.toList)
    val start = lines.zipWithIndex
      .find((l, row) => l.indexOf("S") >= 0)
      .map((l, row) => (row, l.indexOf("S")))
      .get
    val steps = 26501365
    val size = lines.size

    val gridWidth = ((steps / size) - 1)
    val oddGrids = Math.pow(gridWidth / 2 * 2 + 1, 2).toLong
    val evenGrids = Math.pow((gridWidth + 1) / 2 * 2, 2).toLong

    val oddGridPoints = solve(start, grid, size * 2 + 1).toLong
    val evenGridPoints = solve(start, grid, size * 2).toLong

    val northGrid = solve((size - 1, start._2), grid, size - 1).toLong
    val eastGrid = solve((start._1, 0), grid, size - 1).toLong
    val southGrid = solve((0, start._2), grid, size - 1).toLong
    val westGrid = solve((start._1, size - 1), grid, size - 1).toLong

    val smallNorthEast = solve((size - 1, 0), grid, size / 2 - 1).toLong
    val smallNorthWest = solve((size - 1, size - 1), grid, size / 2 - 1).toLong
    val smallSouthEast = solve((0, 0), grid, size / 2 - 1).toLong
    val smallSouthWest = solve((0, size - 1), grid, size / 2 - 1).toLong

    val bigNorthEast = solve((size - 1, 0), grid, size * 3 / 2 - 1).toLong
    val bigNorthWest =
      solve((size - 1, size - 1), grid, size * 3 / 2 - 1).toLong
    val bigSouthEast = solve((0, 0), grid, size * 3 / 2 - 1).toLong
    val bigSouthWest = solve((0, size - 1), grid, size * 3 / 2 - 1).toLong

    oddGrids * oddGridPoints
      + evenGrids * evenGridPoints
      + northGrid + eastGrid + southGrid + westGrid
      + (gridWidth + 1) * (smallNorthEast + smallNorthWest + smallSouthEast + smallSouthWest)
      + gridWidth * (bigNorthEast + bigNorthWest + bigSouthEast + bigSouthWest)
  }
}
