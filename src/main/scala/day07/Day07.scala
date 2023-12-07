package fr.gplassard.adventofcode
package day07

object Day07 {
  private val cardRank: Seq[Char] = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

  enum HandType {
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPairs, OnePair, HighCard
  }

  implicit object HandOrdering extends Ordering[Hand] {
    def compare(a: Hand, b: Hand): Int = a.compareTo(b)
  }

  case class Hand(cards: List[Char]) {
    def handType(): HandType = {
      cards.groupBy(identity).view.mapValues(_.size) match {
        case m if m.values.max == 5 => HandType.FiveOfAKind
        case m if m.values.max == 4 => HandType.FourOfAKind
        case m if m.size == 2 && m.values.max == 3 => HandType.FullHouse
        case m if m.values.max == 3 => HandType.ThreeOfAKind
        case m if m.size == 3 && m.values.max == 2 => HandType.TwoPairs
        case m if m.values.max == 2 => HandType.OnePair
        case _ => HandType.HighCard
      }
    }

    def compareTo(other: Hand): Int = {
      val handRankDiff = this.handType().ordinal - other.handType().ordinal
      if (handRankDiff != 0) handRankDiff
      else {
        this.cards.map(cardRank.indexOf(_))
          .zip(other.cards.map(cardRank.indexOf(_)))
          .map(x => x._1 - x._2)
          .find(_ != 0)
          .getOrElse(0)
      }
    }

  }

  def part1(lines: List[String]): Int = {
    lines
      .map { line =>
        val hand = Hand(line.split(" ")(0).toList)
        val bid = line.split(" ")(1).toInt
        (hand, bid)
      }
      .sortBy((hand, _) => hand)
      .reverse
      .zipWithIndex
      .map((pair, index) => pair._2 * (index + 1))
      .sum
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
