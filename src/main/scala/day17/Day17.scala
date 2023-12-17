package fr.gplassard.adventofcode
package day17

import scala.collection.mutable.{Map => MMap, PriorityQueue => MPriorityQueue}

object Day17 {
  enum Direction {
    case CONTINUE, LEFT, RIGHT
  }
  case class Position(row: Int, col: Int, previousDirections: List[Direction])

  def part1(lines: List[String]): Int =
    djikstra(lines.map(_.toList.map(_.toString).map(Integer.parseInt)))

  def djikstra(grid: List[List[Int]]): Int = {
    val start = Position(0, 0, List.empty)
    val distances = MMap.empty[Position, Int].withDefaultValue(Integer.MAX_VALUE)
    val predecessors = MMap(start -> start)
    distances(start) = 0

    implicit val ordering: Ordering[Position] = Ordering.fromLessThan((a,b) => distances(a) > distances(b)) // we always want the lowest value so we have to reverse
    val toVisit = MPriorityQueue(start)

    while (toVisit.nonEmpty) {
      val node = toVisit.dequeue()
      val neighbors = findNeighbors(node, predecessors(node), grid)
      for {neighbor <- neighbors} {
        val newDist = distances(node) + grid(neighbor.row)(neighbor.col)
        if (newDist < distances(neighbor)) {
          distances(neighbor) = newDist
          predecessors(neighbor) = node
          toVisit += neighbor
          if (neighbor.row == grid.length - 1 && neighbor.col == grid.head.length - 1) {
//            debug(grid, predecessors, neighbor, distances)
            return distances(neighbor)
          }
        }
      }
    }
    -1
  }

  def debug(grid: List[List[Int]], predecessors: MMap[Position, Position], end: Position, distances: MMap[Position, Int]) = {
    val simplifiedDistances = distances.groupBy(p => (p._1.row, p._1.col)).map((rowCol, positions) => (rowCol, positions.values.min))
    val lines = for {
      row <- grid.indices
    } yield for {
      col <- grid(row).indices
    } yield simplifiedDistances.getOrElse((row, col), 0)
    lines.foreach(l => println(l.map(i => String.format("%02d", i)).mkString(" ")))
    var current = end
    while (current.col != 0 || current.row != 0) {
      println(s"$current ${distances(current)}")
      current = predecessors(current)
    }
    println(s"$current ${distances(current)}")
  }

  def findNeighbors(current: Position, previous: Position, grid: List[List[Int]]): List[Position] = {
    val direction = findDirection(previous, current)

    val goLeft: Position = (direction match {
      case "NORTH" => current.copy(row = current.row, col = current.col - 1)
      case "EAST"  => current.copy(row = current.row - 1, col = current.col)
      case "SOUTH" => current.copy(row = current.row, col = current.col + 1)
      case "WEST"  => current.copy(row = current.row + 1, col = current.col)
    }).copy(previousDirections = (Direction.LEFT +: current.previousDirections).take(3))

    val goRight: Position = (direction match {
      case "NORTH" => current.copy(row = current.row, col = current.col + 1)
      case "EAST" => current.copy(row = current.row + 1, col = current.col)
      case "SOUTH" => current.copy(row = current.row, col = current.col - 1)
      case "WEST" => current.copy(row = current.row - 1, col = current.col)
    }).copy(previousDirections = (Direction.RIGHT +: current.previousDirections).take(3))

    val continue: Position = (direction match {
      case "NORTH" => current.copy(row = current.row - 1, col = current.col)
      case "EAST" => current.copy(row = current.row, col = current.col + 1)
      case "SOUTH" => current.copy(row = current.row + 1, col = current.col)
      case "WEST" => current.copy(row = current.row, col = current.col - 1)
    }).copy(previousDirections = (Direction.CONTINUE +: current.previousDirections).take(3))
    List(goLeft, goRight, continue)
      .filter(_.previousDirections != List(Direction.CONTINUE, Direction.CONTINUE, Direction.CONTINUE))
      .filter(_.row >= 0)
      .filter(_.row < grid.size)
      .filter(_.col >= 0)
      .filter(_.col < grid.head.size)
  }

  def findDirection(previous: Position, current: Position): String = {
    if (previous.row == current.row) {
      if (previous.col > current.col) "WEST" else "EAST"
    } else {
      if (previous.row > current.row) "NORTH" else "SOUTH"
    }
  }

  def part2(lines: List[String]): Int = -1
}
