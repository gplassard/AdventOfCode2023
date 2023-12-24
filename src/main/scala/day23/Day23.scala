package fr.gplassard.adventofcode
package day23

import scala.collection.mutable.{Queue => MQueue}

object Day23 {
  def part1(lines: List[String]): Int = {
    val grid = lines.map(_.toCharArray.toList)
    val start = (0, lines.head.indexWhere(_ == '.'))
    val end = (lines.length - 1, lines.last.indexWhere(_ == '.'))
    longestPath(start, end, grid)
  }

  def longestPath(from: (Int, Int), to: (Int, Int), grid: List[List[Char]]): Int = {
    val queue = MQueue((from, 0, Set.empty[(Int, Int)]))
    var maxDistance = -1
    while (queue.nonEmpty) {
      val (position, distance, visited) = queue.dequeue()
      if (position == to) {
        maxDistance = Math.max(maxDistance, distance)
      } else {
        val neighbors = findNeighbors(position, grid).filterNot(visited.contains)
        neighbors.foreach(n => queue.enqueue((n, distance + 1, visited + position)))
      }
    }
    maxDistance
  }

  def findNeighbors(point: (Int, Int), grid: List[List[Char]]): List[(Int, Int)] =
    val base = grid(point._1)(point._2) match {
      case '>' => List((0, 1))
      case '<' => List((0, -1))
      case '^' => List((-1, 0))
      case 'v' => List((1, 0))
      case _ =>  List((0, 1),(0, -1),(1, 0), (-1, 0))
    }
    base
      .map(delta => (point._1 + delta._1, point._2 + delta._2))
      .filter(p => p._1 >= 0 && p._1 < grid.length && p._2 >= 0 && p._2 < grid.head.length)
      .filter(p => grid(p._1)(p._2) != '#')

  def part2(lines: List[String]): Int = {
    -1
  }
}
