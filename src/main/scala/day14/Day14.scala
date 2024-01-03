package fr.gplassard.adventofcode
package day14

import scala.collection.mutable.{ListBuffer => MList, Set => MSet}

object Day14 {
  def part1(lines: List[String]): Int =
    val board = tiltToNorth(lines.map(_.toList))
    computeWeight(board)

  private def computeWeight(board: List[List[Char]]): Int =
    board.reverse.zipWithIndex
      .map((line, index) => line.count(_ == 'O') * (index + 1))
      .sum

  def tiltToNorth(board: List[List[Char]]): List[List[Char]] =
    val mutable = MList.from(board.map(MList.from))
    for {
      row <- 0 until mutable.length - 1
      col <- mutable(row).indices
      if mutable(row)(col) == '.'
      switchRow = roundRockRowIndex(mutable, col, row)
      if switchRow.isDefined
    } {
      mutable(row)(col) = 'O'
      mutable(switchRow.get)(col) = '.'
    }
    mutable.toList.map(_.toList)

  def roundRockRowIndex(
      board: MList[MList[Char]],
      column: Int,
      startRow: Int
  ): Option[Int] =
    (startRow + 1 until board.length)
      .takeWhile(row => board(row)(column) != '#')
      .find(row => (board(row)(column) == 'O'))

  private def rotate(twoDList: List[List[Char]]): List[List[Char]] = for {
    col <- twoDList(0).zipWithIndex.map(_._2)
  } yield for {
    row <- twoDList.zipWithIndex.map(_._2).reverse
  } yield twoDList(row)(col)

  def part2(lines: List[String]): Int = {
    var board = lines.map(_.toList)
    val hashes = MSet.empty[Int]
    val boards = MList.empty[List[List[Char]]]
    while (!hashes.contains(board.hashCode())) {
      boards += board
      hashes += board.hashCode()
      board = cycle(board)
    }
    val startOfRecycling =
      boards.map(_.hashCode()).indexWhere(_ == board.hashCode())
    val recyclingLength = boards.length - startOfRecycling
    computeWeight(
      boards.drop(startOfRecycling)(
        (1000000000 - startOfRecycling) % recyclingLength
      )
    )
  }

  def cycle(board: List[List[Char]]): List[List[Char]] =
    rotate(
      tiltToNorth(
        rotate(tiltToNorth(rotate(tiltToNorth(rotate(tiltToNorth(board))))))
      )
    )
}
