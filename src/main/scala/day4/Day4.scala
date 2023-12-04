package fr.gplassard.adventofcode
package day4

object Day4 {

  def part1(lines: List[String]): Int =
    lines.map { line =>
      val lists = line.split(":")(1).split("\\|")
      val winning = lists(0).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
      val cards = lists(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
      val intersect = winning.intersect(cards)
      Math.pow(2, intersect.size - 1).toInt
    }.sum

}
