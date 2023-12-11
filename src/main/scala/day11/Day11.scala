package fr.gplassard.adventofcode
package day11

import scala.math.Ordered.orderingToOrdered

object Day11 {
  def part1(lines: List[String]): Int = {
    val galaxy = lines.map(_.toList)
    val expandedRowsIndices = lines.zipWithIndex.filter(l => l._1.forall(_ == '.')).map(_._2).toSet
    val expandedColsIndices = rotate(galaxy).zipWithIndex.filter(l => l._1.forall(_ == '.')).map(_._2).toSet
    val galaxyPositions = starsPositions(galaxy).toSet
    val distinctPairs = cartesianProduct(galaxyPositions, galaxyPositions)
      .filter((a, b) => a != b)
      .map((a, b) => if (a > b) (b, a) else (a,b))
      .toList
      .sorted
    distinctPairs
      .map((a, b) => computeDistance(a, b, expandedRowsIndices, expandedColsIndices))
      .sum
  }

  private def starsPositions(galaxy: List[List[Char]]): List[(Int, Int)] =
    for {
      (row, rowIndex) <- galaxy.zipWithIndex
      (point, colIndex) <- row.zipWithIndex
      if point == '#'
    } yield (rowIndex, colIndex)

  private def cartesianProduct(as: Set[(Int, Int)], bs: Set[(Int, Int)]): Set[((Int, Int), (Int, Int))] = for {a <- as; b <- bs} yield (a, b)

  private def computeDistance(a: (Int, Int), b:(Int, Int), expandedRowsIndices: Set[Int], expandedColsIndices: Set[Int]): Int =
    val deltaR = Math.abs(b._1 - a._1)
    val deltaC = Math.abs(b._2 - a._2)
    val doubleR = expandedRowsIndices.filter(r => Math.min(a._1, b._1) < r && r < Math.max(a._1, b._1))
    val doubleC = expandedColsIndices.filter(c => Math.min(a._2, b._2) < c && c < Math.max(a._2, b._2))
    deltaR + deltaC + doubleR.size + doubleC.size

  private def rotate(galaxy: List[List[Char]]): List[List[Char]] =
    for {
      col <- galaxy(0).zipWithIndex.map(_._2)
    } yield for {
      row <- galaxy.zipWithIndex.map(_._2)
    } yield galaxy(row)(col)


  def part2(lines: List[String]): Int = {
    -1
  }
}
