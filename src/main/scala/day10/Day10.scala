package fr.gplassard.adventofcode
package day10

import scala.collection.mutable.{Stack => MStack, Map => MMap}

object Day10 {
  def part1(lines: List[String]): Int = {
    val startPosition = findStartPosition(lines)
    val maze = lines.map(_.toList)
    cycleLength(startPosition, maze)._2
  }

  private def findStartPosition(lines: List[String]): (Int, Int) = {
    val row = lines.zipWithIndex.find(_._1.contains("S")).get._2
    (row, lines(row).lastIndexOf('S'))
  }

  def cycleLength(startPosition: (Int, Int), maze: List[List[Char]]): (MMap[(Int, Int), Int], Int) = {
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
    (dist, dist.values.max)
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
    val dists = cycleLength(findStartPosition(lines), lines.map(_.toList))._1
    var area = 0
    for ((row, rowIndex) <- lines.map(_.toList).zipWithIndex) {
      var inside = false // scan line by line and flip each time we encounter a pipe that switches from inside to outside
      //var line = List.empty[Char]
      for ((col, colIndex) <- row.zipWithIndex) {
        val pipe = col
        val isOnPipe = dists.contains((rowIndex, colIndex))

        if (isOnPipe && Set('|', '7', 'F', 'S').contains(pipe)) {
          inside = !inside
        }
        if (inside  && !isOnPipe) {
          area += 1
        }
        //line = line appended (if (isOnPipe) col else (if (inside) 'D' else 'O'))
      }
      //println(line.mkString)
    }
    area
  }
}
