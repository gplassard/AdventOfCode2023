package fr.gplassard.adventofcode
package day08

object Day08 {

  def part1(lines: List[String]): Int =
    val instructions = lines(0).toList
    val paths = makePaths(lines)
    count("AAA", instructions, paths, s => s == "ZZZ")

  private def count(
      start: String,
      instructions: List[Char],
      paths: Map[String, (String, String)],
      stop: String => Boolean
  ): Int =
    var (current, count) = (start, 0)
    while (!stop(current)) {
      val instruction = instructions(count % instructions.size)
      current = nextPosition(paths, current, instruction)
      count += 1
    }
    count

  private def nextPosition(
      paths: Map[String, (String, String)],
      current: String,
      instruction: Char
  ): String =
    if (instruction == 'L') paths(current)._1 else paths(current)._2

  private def makePaths(lines: List[String]): Map[String, (String, String)] =
    lines
      .drop(2)
      .map { line =>
        val src = line.split(" = ")(0)
        val dest =
          line.split(" = ")(1).replace("(", "").replace(")", "").split(", ")
        val left = dest(0).trim
        val right = dest(1).trim
        src -> (left, right)
      }
      .toMap

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) { (a, b) =>
    b * a / LazyList
      .iterate((a, b)) { case (x, y) => (y, x % y) }
      .dropWhile(_._2 != 0)
      .head
      ._1
      .abs
  }

  def part2(lines: List[String]): BigInt =
    val instructions = lines(0).toList
    val paths = makePaths(lines)
    val counts = paths.keys
      .filter(_.endsWith("A"))
      .map(position =>
        count(position, instructions, paths, s => s.endsWith("Z"))
      )
      .map(BigInt.apply)
      .toSeq
    lcm(counts)
}
