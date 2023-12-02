package fr.gplassard.adventofcode
package day2

object Day2 {

  def part1(lines: List[String]): Int =
    val gameIds = for {
      line <- lines
      gameId = line.split(":")(0).replace("Game", "").trim.toInt
      if isValidGame(line.split(":")(1).split(";").toList)
    } yield gameId
    gameIds.sum

  def isValidGame(sets: List[String]): Boolean = sets.forall(isValidSet)

  def isValidSet(set: String): Boolean = {
    val reveals = set.split(",")
    val blues = reveals.filter(_.contains("blue")).map(_.replaceAll("blue", "").trim.toInt).maxOption.getOrElse(0)
    val reds = reveals.filter(_.contains("red")).map(_.replaceAll("red", "").trim.toInt).maxOption.getOrElse(0)
    val greens = reveals.filter(_.contains("green")).map(_.replaceAll("green", "").trim.toInt).maxOption.getOrElse(0)
    reds <= 12 && greens <= 13 && blues <= 14
  }
}
