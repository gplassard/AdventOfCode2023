package fr.gplassard.adventofcode
package day18

object Day18 {
  enum Direction(val deltaRow: Int, val deltaColumn: Int) {
    case R extends Direction(0, 1)
    case D extends Direction(1, 0)
    case L extends Direction(0, -1)
    case U extends Direction(-1, 0)
  }
  case class PathElement(row: BigInt, col: BigInt)

  def part1(lines: List[String]): Int = {
    val instructions = lines.map(l => {
      val direction = Direction.valueOf(l.split(" ")(0))
      val delta = BigInt(l.split(" ")(1))
      (direction, delta)
    })
    solve(instructions).toInt
  }

  def solve(instructions: List[(Direction, BigInt)]): BigInt = {
    val start = PathElement(0, 0)
    val path = instructions.scanLeft(start)((lastPoint, value) => {
      val (direction, delta) = value
      PathElement(
        lastPoint.row + direction.deltaRow * delta,
        lastPoint.col + direction.deltaColumn * delta
      )
    })
    val totalPoints = instructions.map(_._2).sum
    val shoeLace = path.indices
      .map(i =>
        path(i).row * (path(if (i >= 1) i - 1 else path.length - 1).col - path(
          (i + 1) % path.length
        ).col)
      )
      .sum / 2
    val abs = if (shoeLace < 0) -shoeLace else shoeLace
    val i = abs - (totalPoints / 2) + 1
    i + totalPoints
  }

  def part2(lines: List[String]): BigInt = {
    val instructions = lines.map(l => {
      val direction = Direction.fromOrdinal(l.dropRight(1).last.toString.toInt)
      val delta = BigInt(l.split("#")(1).dropRight(2), 16)
      (direction, delta)
    })
    solve(instructions)
  }
}
