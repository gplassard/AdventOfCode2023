package fr.gplassard.adventofcode
package day14

import scala.collection.mutable.{ListBuffer => MList}

object Day14 {
  def part1(lines: List[String]): Int = {
    val board = tiltToNorth(lines.map(_.toList))
    board
      .reverse
      .zipWithIndex
      .map((line, index) => line.count(_ == 'O') * (index + 1))
      .sum
  }

  def tiltToNorth(board: List[List[Char]]): List[List[Char]] =
    val mutable = MList.from(board.map(MList.from))
    var hasChanged = true
    while (hasChanged) {
      hasChanged = false
      for {
        row <- 1 until mutable.length
        col <- mutable(row).indices
        if (mutable(row)(col) == 'O' && mutable(row - 1)(col) == '.')
      } {
        mutable(row - 1)(col) = 'O'
        mutable(row)(col) = '.'
        hasChanged = true
      }
    }
    mutable.toList.map(_.toList)


  def part2(lines: List[String]): Int = {
    -1
  }
}
