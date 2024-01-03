package fr.gplassard.adventofcode
package day06

object Day06 {

  def computeDistance(waitingTime: Long, totalTime: Long): Long =
    waitingTime * (totalTime - waitingTime)

  def part1(lines: List[String]): Long =
    val times =
      lines(0).replace("Time:", "").split(" ").filter(_.nonEmpty).map(_.toInt)
    val distances = lines(1)
      .replace("Distance:", "")
      .split(" ")
      .filter(_.nonEmpty)
      .map(_.toInt)
    val races = times.zip(distances)
    val solved = races.map(solveRace)
    solved.product

  def solveRace(race: (Int, Int)): Int =
    val (time, raceDistance) = race
    (0 to time).count(waitingTime =>
      computeDistance(waitingTime, time) > raceDistance
    )

  def part2(lines: List[String]): Long =
    val totalTime = lines(0).replace("Time:", "").replaceAll(" ", "").toLong
    val distance = lines(1).replace("Distance:", "").replaceAll(" ", "").toLong
    val first = binarySearch(
      0,
      totalTime / 2,
      distance,
      waitingTime => computeDistance(waitingTime, totalTime)
    )
    totalTime + 1 - (2 * first)

  def binarySearch(
      start: Long,
      end: Long,
      value: Long,
      transform: Long => Long
  ): Long =
    if (start == end) return start
    val pivot = transform(start + (end - start) / 2)
    if (pivot < value)
      binarySearch(start + (end - start) / 2 + 1, end, value, transform)
    else binarySearch(start, start + (end - start) / 2, value, transform)
}
