package fr.gplassard.adventofcode
package day09

object Day09 {
  def part1(lines: List[String]): Int =
    lines.map(line => solveSerie(line.split(" ").map(_.trim.toInt).toList)).sum

  def solveSerie(serie: List[Int]): Int = {
    if (serie.forall(_ == 0)) return 0
    val derivative = serie.tail.zip(serie).map(t => t._1 - t._2)
    serie.last + solveSerie(derivative)
  }

  def part2(lines: List[String]): Int =
    lines.map(line => solveSeriePart2(line.split(" ").map(_.trim.toInt).toList)).sum

  def solveSeriePart2(serie: List[Int]): Int = {
    if (serie.forall(_ == 0)) return 0
    val derivative = serie.tail.zip(serie).map(t => t._1 - t._2)
    serie.head - solveSeriePart2(derivative)
  }
}
