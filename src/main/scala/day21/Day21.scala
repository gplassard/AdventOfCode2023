package fr.gplassard.adventofcode
package day21

import scala.collection.mutable.{Set => MSet, Queue => MQueue}

object Day21 {
  def part1(lines: List[String]): Int = {
    val grid = lines.map(_.toCharArray.toList)
    val start = lines.zipWithIndex.find((l, row) => l.indexOf("S") >= 0).map((l, row) => (row, l.indexOf("S"))).get
    solve(start, grid, 64)
  }

  def findNeighbors(point: (Int, Int), grid: List[List[Char]]): List[(Int, Int)] =
    List((0, 1),(0, -1),(1, 0), (-1, 0))
      .map(delta => (point._1 + delta._1, point._2 + delta._2))
      .filter(p => p._1 >= 0 && p._1 < grid.length && p._2 >= 0 && p._2 < grid.head.length)
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

  def part2(lines: List[String]): Int = {
    -1
  }
}
