package fr.gplassard.adventofcode
package day16

import day16.Day16.Direction.*

import scala.collection.mutable.{Set as MSet, Stack as MStack}

object Day16 {
  enum Direction(
      val deltaRow: Int,
      val deltaColumn: Int,
      val isHorizontal: Boolean
  ) {
    case LEFT extends Direction(0, -1, true)
    case RIGHT extends Direction(0, 1, true)
    case UP extends Direction(-1, 0, false)
    case DOWN extends Direction(1, 0, false)
  }
  case class Beam(x: Int, y: Int, direction: Direction)

  def part1(lines: List[String]): Int = solve(lines, Beam(0, -1, RIGHT))

  def solve(lines: List[String], start: Beam): Int = {
    val grid = lines.map(_.toList)
    val beams = MSet.empty[Beam]
    val toContinue = MStack(start)
    while (toContinue.nonEmpty) {
      var beam: Option[Beam] = Some(toContinue.pop())
      while (beam.isDefined && !beams.contains(beam.get)) {
        beams += beam.get
        val (nextBeam, eventualNewBeam) = next(beam.get, grid)
        beam = nextBeam
        eventualNewBeam.foreach(toContinue += _)
      }
    }
    val energized = beams.map(b => (b.x, b.y))
    energized.size - 1
  }

  def next(beam: Beam, grid: List[List[Char]]): (Option[Beam], Option[Beam]) = {
    val nextPosition =
      (beam.x + beam.direction.deltaRow, beam.y + beam.direction.deltaColumn)
    if (!inBound(nextPosition, grid)) {
      return (None, None)
    }
    val nextAt = grid(nextPosition._1)(nextPosition._2)
    val (first, second) = nextAt match {
      case '-' if !beam.direction.isHorizontal => (LEFT, Some(RIGHT))
      case '|' if beam.direction.isHorizontal  => (UP, Some(DOWN))
      case '/' if beam.direction == LEFT       => (DOWN, None)
      case '/' if beam.direction == RIGHT      => (UP, None)
      case '/' if beam.direction == UP         => (RIGHT, None)
      case '/' if beam.direction == DOWN       => (LEFT, None)
      case '\\' if beam.direction == LEFT      => (UP, None)
      case '\\' if beam.direction == RIGHT     => (DOWN, None)
      case '\\' if beam.direction == UP        => (LEFT, None)
      case '\\' if beam.direction == DOWN      => (RIGHT, None)
      case _                                   => (beam.direction, None)
    }
    (
      Some(Beam(nextPosition._1, nextPosition._2, first)),
      second.map(s => Beam(nextPosition._1, nextPosition._2, s))
    )
  }

  def inBound(tuple: (Int, Int), grid: List[List[Char]]): Boolean =
    tuple._1 >= 0 && tuple._2 >= 0 && tuple._1 < grid.size && tuple._2 < grid.head.size

  def part2(lines: List[String]): Int = {
    val possibleStarts =
      lines.indices.map(row => Beam(row, -1, RIGHT))
        ++ lines.indices.map(row => Beam(row, lines.head.length, LEFT))
        ++ lines.head.indices.map(col => Beam(-1, col, DOWN))
        ++ lines.head.indices.map(col => Beam(lines.length, col, UP))
    possibleStarts.map(start => solve(lines, start)).max
  }
}
