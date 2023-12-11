package fr.gplassard.adventofcode
package day11

import scala.math.Ordered.orderingToOrdered

object Day11 {
  def part1(lines: List[String]): BigInt = solve(lines, BigInt(2))

  def solve(lines: List[String], multiplier: BigInt): BigInt = {
    val galaxy = lines.map(_.toList)
    val expandedRowsIndices = lines.zipWithIndex.filter(l => l._1.forall(_ == '.')).map(_._2).map(BigInt.apply).toSet
    val expandedColsIndices = rotate(galaxy).zipWithIndex.filter(l => l._1.forall(_ == '.')).map(_._2).map(BigInt.apply).toSet
    val galaxyPositions = starsPositions(galaxy).toSet
    val distinctPairs = cartesianProduct(galaxyPositions, galaxyPositions)
      .filter((a, b) => a != b)
      .map((a, b) => if (a > b) (b, a) else (a,b))
      .toList
      .sorted
    distinctPairs
      .map((a, b) => computeDistance(a, b, expandedRowsIndices, expandedColsIndices, multiplier))
      .sum
  }

  private def starsPositions(galaxy: List[List[Char]]): List[(BigInt, BigInt)] =
    for {
      (row, rowIndex) <- galaxy.zipWithIndex
      (point, colIndex) <- row.zipWithIndex
      if point == '#'
    } yield (rowIndex, colIndex)

  private def cartesianProduct(as: Set[(BigInt, BigInt)], bs: Set[(BigInt, BigInt)]): Set[((BigInt, BigInt), (BigInt, BigInt))] = for {a <- as; b <- bs} yield (a, b)

  private def computeDistance(a: (BigInt, BigInt), b:(BigInt, BigInt), expandedRowsIndices: Set[BigInt], expandedColsIndices: Set[BigInt], multiplier: BigInt): BigInt =
    val deltaR = abs(b._1 - a._1)
    val deltaC = abs(b._2 - a._2)
    val doubleR = expandedRowsIndices.filter(r => min(a._1, b._1) < r && r < max(a._1, b._1))
    val doubleC = expandedColsIndices.filter(c => min(a._2, b._2) < c && c < max(a._2, b._2))
    deltaR - doubleR.size + deltaC - doubleC.size + multiplier * (doubleR.size + doubleC.size)

  private def abs(bInt: BigInt): BigInt = if (bInt > 0) bInt else - bInt
  private def min(a: BigInt, b: BigInt): BigInt = if (a > b) b else a
  private def max(a: BigInt, b: BigInt): BigInt = if (a > b) a else b

  private def rotate(galaxy: List[List[Char]]): List[List[Char]] =
    for {
      col <- galaxy(0).zipWithIndex.map(_._2)
    } yield for {
      row <- galaxy.zipWithIndex.map(_._2)
    } yield galaxy(row)(col)


  def part2(lines: List[String]): BigInt = solve(lines, BigInt(1000000))
}
