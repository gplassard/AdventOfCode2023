package fr.gplassard.adventofcode
package day18

object Day18 {
  enum Direction(val deltaRow: Int, val deltaColumn: Int) {
    case L extends Direction(0, -1)
    case R extends Direction(0, 1)
    case U extends Direction(-1, 0)
    case D extends Direction(1, 0)
  }
  case class PathElement(row: Int, col: Int)

  def part1(lines: List[String]): Int = {
    val instructions = lines.map(l => {
      val direction = Direction.valueOf(l.split(" ")(0))
      val delta = Integer.parseInt(l.split(" ")(1))
      (direction, delta)
    })
    val start = PathElement(0, 0)
    val path = instructions.scanLeft(start)((lastPoint, value) => {
      val (direction, delta) = value
      PathElement(lastPoint.row + direction.deltaRow * delta, lastPoint.col + direction.deltaColumn * delta)
    })
    val totalPoints = instructions.map(_._2).sum
    val shoeLace = Math.abs(path.indices.map(i => path(i).row * (path(if (i >= 1) i - 1 else path.length - 1).col - path((i + 1) % path.length).col)).sum) / 2
    val i = shoeLace - (totalPoints / 2) + 1
    i + totalPoints
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
