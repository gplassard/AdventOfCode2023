package fr.gplassard.adventofcode
package day6


object Day6 {

  def distance(waitingTime: Int, totalTime: Int): Int = waitingTime * (totalTime - waitingTime)

  def part1(lines: List[String]): Long =
    val times = lines(0).replace("Time:", "").split(" ").filter(_.nonEmpty).map(_.toInt)
    val distances = lines(1).replace("Distance:", "").split(" ").filter(_.nonEmpty).map(_.toInt)
    val races = times.zip(distances)
    val solved = races.map(solveRace)
    solved.product

  def solveRace(race: (Int, Int)): Int =
    val (time, raceDistance) = race
    (0 to time).count(waitingTime =>distance(waitingTime, time) > raceDistance)

}
