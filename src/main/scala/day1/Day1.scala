package fr.gplassard.adventofcode
package day1

object Day1 {

  def part1(lines: List[String]): Int =
    lines.map( word =>
      val digits = word.filter(c => c.isDigit)
      Integer.parseInt(s"${digits.head}${digits.last}")
    ).sum
}
