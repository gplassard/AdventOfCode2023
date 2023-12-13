package fr.gplassard.adventofcode
package day13

import scala.collection.mutable.ListBuffer as MList

object Day13 {
  def part1(lines: List[String]): Int =
    val patterns = preparePatterns(lines)
    patterns
      .map(_.map(_.toList).toList)
      .filter(_.nonEmpty)
      .map(findReflection)
      .map((rowsCounts, columnsCounts) => rowsCounts.getOrElse(0) + columnsCounts.getOrElse(0))
      .sum

  private def preparePatterns(lines: List[String]): MList[MList[String]] = {
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

  def part2(lines: List[String]): Int = {
    -1
  }
}
