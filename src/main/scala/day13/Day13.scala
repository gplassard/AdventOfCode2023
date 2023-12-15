package fr.gplassard.adventofcode
package day13

import scala.collection.mutable.ListBuffer as MList

object Day13 {
  def part1(lines: List[String]): Int =
    val patterns = preparePatterns(lines)
    patterns
      .map(findReflection)
      .map((rowsCounts, columnsCounts) => rowsCounts.getOrElse(0) + columnsCounts.getOrElse(0))
      .sum

  private def preparePatterns(lines: List[String]): List[List[List[Char]]] = {
    var currentList = MList.empty[String]
    val patterns = MList(currentList)
    for {
      line <- lines.appended("")
    } {
      if (line.isBlank) {
        currentList = MList.empty
        patterns.addOne(currentList)
      }
      else {
        currentList.addOne(line)
      }
    }
    patterns
      .map(_.map(_.toList).toList)
      .filter(_.nonEmpty)
      .toList
  }

  private def findReflection(pattern: List[List[Char]]): (Option[Int], Option[Int]) =
    val row = (0 until pattern.length - 1).find(index => isReflectionIndex(index, pattern))
    val col = (0 until rotate(pattern).length - 1).find(index => isReflectionIndex(index, rotate(pattern)))
    (row.map(_ + 1).map(_ * 100), col.map(_ + 1))

  private def rotate(twoDList: List[List[Char]]): List[List[Char]] = for {
    col <- twoDList(0).zipWithIndex.map(_._2)
  } yield for {
    row <- twoDList.zipWithIndex.map(_._2)
  } yield twoDList(row)(col)


  private def isReflectionIndex(index: Int, pattern: List[List[Char]]): Boolean =
    val max = Math.min(index, (pattern.length) - index - 2)
    (0 to max)
      .forall(delta => {
        pattern(index - delta) == pattern(index + delta + 1)
      })

  private def isAlmostReflectionIndex(index: Int, pattern: List[List[Char]]): Boolean =
    val max = Math.min(index, (pattern.length) - index - 2)
    (0 to max)
      .count(delta => {
        pattern(index - delta) != pattern(index + delta + 1)
      }) == 1

  def part2(lines: List[String]): Int =
    val patterns = preparePatterns(lines)
    patterns
      .map(unsmudged)
      .map((rowsCounts, columnsCounts) => rowsCounts.getOrElse(0) + columnsCounts.getOrElse(0))
      .sum

  def unsmudged(pattern: List[List[Char]]): (Option[Int], Option[Int]) = {
    val row = for {
      index <- 0 until pattern.length - 1
      delta <- 0 to Math.min(index, pattern.length - index - 2)
      if countDifferences(pattern(index - delta), pattern(index + delta + 1)) == 1
      if isAlmostReflectionIndex(index, pattern)
    } yield (index + 1) * 100

    val rotated = rotate(pattern)
    val col = for {
      index <- 0 until rotated.length - 1
      delta <- 0 to Math.min(index, rotated.length - index - 2)
      if countDifferences(rotated(index - delta), rotated(index + delta + 1)) == 1
      if isAlmostReflectionIndex(index, rotate(pattern))
    } yield index + 1

    (row.headOption, col.headOption)
  }

  private def countDifferences(as: List[Char], bs: List[Char]) = as.zip(bs).map((a, b) => if (a == b) 0 else 1).sum
}
