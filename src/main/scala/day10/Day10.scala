package fr.gplassard.adventofcode
package day10

import scala.collection.mutable.{Stack => MStack, Map => MMap}

object Day10 {
  def part1(lines: List[String]): Int = {
    val startPosition = {
      val row = lines.zipWithIndex.find(_._1.contains("S")).get._2
      (row, lines(row).lastIndexOf('S'))
    }
    val maze = lines.map(_.toList)
    cycleLength(startPosition, maze)
  }

  def cycleLength(startPosition: (Int, Int), maze: List[List[Char]]): Int = {
    val startNeighbors = List((0,1), (0,-1), (1,0), (-1,0))
      .map((dr, dc) => (startPosition._1 + dr, startPosition._2 + dc))
      .filter(candidate => findNeighbors(maze, candidate).contains(startPosition))

    val stack = MStack(startPosition)
    val dist = MMap(
      startPosition -> 0
    )

    while (stack.nonEmpty) {
      val node = stack.pop
      val neighbors = if (node == startPosition) startNeighbors else findNeighbors(maze, node)
      for (neighbor <- neighbors) {
        if (!dist.contains(neighbor)) {
          dist.update(neighbor, dist(node) + 1)
          stack.addOne(neighbor)
        }
      }
    }
    dist.values.max
  }

  def findNeighbors(maze: List[List[Char]], position: (Int, Int)): List[(Int,Int)] = {
    val (row, column) = position
    maze(position._1)(position._2) match {
      case '|' => List((row - 1, column), (row + 1, column))
      case '-' => List((row, column - 1), (row, column + 1))
      case 'L' => List((row - 1, column), (row, column + 1))
      case 'J' => List((row - 1, column), (row, column - 1))
      case '7' => List((row + 1, column), (row, column - 1))
      case 'F' => List((row + 1, column), (row, column + 1))
      case '.' => List.empty
    }
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
