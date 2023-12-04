package fr.gplassard.adventofcode
package day4

object Day4 {

  def part1(lines: List[String]): Int =
    lines.map { line =>
      val lists = line.split(":")(1).split("\\|")
      val winning = lists(0).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
      val cards = lists(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
      val intersect = winning.intersect(cards)
      Math.pow(2, intersect.size - 1).toInt
    }.sum

  case class Card(id: Int, awards: Set[Int])

  def part2(lines: List[String]): Int = {
    val combos = winningCombos(lines).map(card => card.id -> card).toMap
    var totalCards = combos.keys.map(cardId => cardId -> 1).toMap
    var hasNew = true
    var newCards = combos.keys.toList
    var iter = 0
    while (newCards.size > 0) {
      var futureNewCards = List.empty[Int]
      newCards.foreach { cardId =>
        futureNewCards = futureNewCards ++ combos.get(cardId).map(_.awards.toList).getOrElse(List.empty)
      }
      futureNewCards.foreach { cardId =>
        totalCards = totalCards + (cardId -> (totalCards.getOrElse(cardId, 0) + 1))
      }
      newCards = futureNewCards
      println(s"$iter ${futureNewCards.size} ${futureNewCards.take(10)}")
      iter = iter + 1
    }
    totalCards.values.sum
  }

  def winningCombos(lines: List[String]): List[Card] = lines.map { line =>
    val id = line.split(":")(0).replace("Card", "").trim.toInt
    val lists = line.split(":")(1).split("\\|")
    val winning = lists(0).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
    val cards = lists(1).split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet
    val intersect = winning.intersect(cards)
    Card(id, (1 + id to (id + intersect.size)).toSet)
  }

}
