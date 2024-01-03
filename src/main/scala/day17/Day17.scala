package fr.gplassard.adventofcode
package day17

import day17.Day17.Direction.*

import scala.collection.mutable.{PriorityQueue as MPriorityQueue, Set as MSet}

object Day17 {
  enum Direction(val deltaRow: Int, val deltaColumn: Int) {
    case LEFT extends Direction(0, -1)
    case UP extends Direction(-1, 0)
    case RIGHT extends Direction(0, 1)
    case DOWN extends Direction(1, 0)

    def turnLeft(): Direction = Direction.fromOrdinal(
      (Direction.values.length + this.ordinal - 1) % Direction.values.length
    )
    def turnRight(): Direction =
      Direction.fromOrdinal((this.ordinal + 1) % Direction.values.length)
  }

  case class Position(
      row: Int,
      col: Int,
      direction: Direction,
      countContinue: Int
  )

  def part1(lines: List[String]): Int =
    val grid = lines.map(_.toList.map(_.toString).map(Integer.parseInt))
    computeHeatloss(grid, 0, 3)

  def computeHeatloss(
      grid: List[List[Int]],
      minContinue: Int,
      maxContinue: Int
  ): Int = {
    implicit val ordering: Ordering[(Position, Int)] = Ordering
      .by[(Position, Int), Int](_._2)
      .reverse // we always want the lowest value so we have to reverse
    val seen = MSet.empty[Position]
    val toVisit = MPriorityQueue(
      (Position(0, 0, RIGHT, 0), 0),
      (Position(0, 0, DOWN, 0), 0)
    )

    while (toVisit.nonEmpty) {
      val (current, heatloss) = toVisit.dequeue()
      if (
        current.row == grid.size - 1 && current.col == grid.head.size - 1 && current.countContinue >= minContinue
      ) {
        return heatloss
      }
      if (!seen.contains(current)) {
        seen.add(current)
        val neighbors = findNeighbors(current, grid, minContinue, maxContinue)
        neighbors.foreach { neighbor =>
          val delta =
            if (neighbor.countContinue != 0) grid(neighbor.row)(neighbor.col)
            else 0
          toVisit += ((neighbor, heatloss + delta))
        }
      }
    }
    -1
  }

  def findNeighbors(
      current: Position,
      grid: List[List[Int]],
      minContinue: Int,
      maxContinue: Int
  ): List[Position] = {
    val left =
      current.copy(direction = current.direction.turnLeft(), countContinue = 0)
    val right =
      current.copy(direction = current.direction.turnRight(), countContinue = 0)
    val continue = current.copy(
      row = current.row + current.direction.deltaRow,
      col = current.col + current.direction.deltaColumn,
      countContinue = current.countContinue + 1
    )
    List(left, right, continue)
      .filter(n => isInGrid(n, grid))
      .filter(n =>
        current.countContinue != 0 || current.direction == n.direction
      ) // don't turn twice in a row
      .filter(n =>
        current.direction == n.direction || current.countContinue >= minContinue
      ) // don't turn before min distance
      .filter(n => n.countContinue <= maxContinue)
  }

  def isInGrid(position: Position, grid: List[List[Int]]): Boolean =
    position.row >= 0
      && position.row < grid.size
      && position.col >= 0
      && position.col < grid.head.length

  def part2(lines: List[String]): Int =
    val grid = lines.map(_.toList.map(_.toString).map(Integer.parseInt))
    computeHeatloss(grid, 4, 10)

}
