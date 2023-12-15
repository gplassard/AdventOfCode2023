package fr.gplassard.adventofcode
package day15

object Day15 {
  def part1(lines: List[String]): Int =
    lines(0).split(",").map(hash).sum

  def hash(text: String): Int =
    text.foldLeft(0)((value, char) => ((value + char.toInt)  * 17) % 256)

  def part2(lines: List[String]): Int = {
    -1
  }
}
