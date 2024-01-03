package fr.gplassard.adventofcode
package day04

object Day04 {

  def part1(lines: List[String]): Int =
    lines.map { line =>
      val lists = line.split(":")(1).split("\\|")
      val winning =
        lists(0).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
      val cards =
        lists(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
      val intersect = winning.intersect(cards)
      Math.pow(2, intersect.size - 1).toInt
    }.sum

  case class Card(id: Int, rewards: Set[Int])

  def part2(lines: List[String]): Int = {
    val cards = winningCombos(lines)
    var cardsCount = cards.map(_ => 1)
    cards.foreach(card => {
      val count = cardsCount(card.id - 1)
      card.rewards.map { reward =>
        val updatedValue = cardsCount(reward - 1) + count
        cardsCount = cardsCount.updated(reward - 1, updatedValue)
      }
    })
    cardsCount.sum
  }

  def winningCombos(lines: List[String]): List[Card] = lines.map { line =>
    val id = line.split(":")(0).replace("Card", "").trim.toInt
    val lists = line.split(":")(1).split("\\|")
    val winning =
      lists(0).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
    val cards =
      lists(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
    val intersect = winning.intersect(cards)
    Card(id, (1 + id to (id + intersect.size)).toSet)
  }

}
