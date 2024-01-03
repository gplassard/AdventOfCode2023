package fr.gplassard.adventofcode
package day07

object Day07 {
  enum HandType {
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPairs, OnePair,
      HighCard
  }

  implicit object HandPart1Ordering extends Ordering[HandPart1] {
    private val cardRank: Seq[Char] =
      List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
    def compare(a: HandPart1, b: HandPart1): Int = a.compareTo(b, cardRank)
  }

  trait Hand {
    val cards: List[Char]
    def handType(): HandType
    def compareTo(other: Hand, cardRank: Seq[Char]): Int = {
      val handRankDiff = this.handType().ordinal - other.handType().ordinal
      if (handRankDiff != 0) handRankDiff
      else {
        this.cards
          .map(cardRank.indexOf(_))
          .zip(other.cards.map(cardRank.indexOf(_)))
          .map(x => x._1 - x._2)
          .find(_ != 0)
          .getOrElse(0)
      }
    }
  }

  case class HandPart1(cards: List[Char]) extends Hand {
    def handType(): HandType = {
      cards.groupBy(identity).view.mapValues(_.size) match {
        case m if m.values.max == 5                => HandType.FiveOfAKind
        case m if m.values.max == 4                => HandType.FourOfAKind
        case m if m.size == 2 && m.values.max == 3 => HandType.FullHouse
        case m if m.values.max == 3                => HandType.ThreeOfAKind
        case m if m.size == 3 && m.values.max == 2 => HandType.TwoPairs
        case m if m.values.max == 2                => HandType.OnePair
        case _                                     => HandType.HighCard
      }
    }
  }

  def part1(lines: List[String]): Int =
    solve(
      lines
        .map { line =>
          val hand = HandPart1(line.split(" ")(0).toList)
          val bid = line.split(" ")(1).toInt
          (hand, bid)
        }
    )

  private def solve[T](
      parsedLines: List[(T, Int)]
  )(implicit ordering: Ordering[T]): Int = {
    parsedLines
      .sortBy((hand, _) => hand)
      .reverse
      .zipWithIndex
      .map((pair, index) => pair._2 * (index + 1))
      .sum
  }

  implicit object HandPart2Ordering extends Ordering[HandPart2] {
    private val cardRank: Seq[Char] =
      List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')
    def compare(a: HandPart2, b: HandPart2): Int = a.compareTo(b, cardRank)
  }

  case class HandPart2(cards: List[Char]) extends Hand {
    def handType(): HandType = {
      val grouped = cards.groupBy(identity)
      val jokerCount = grouped.getOrElse('J', List.empty).size
      val differentCards = grouped.size - (if (jokerCount > 0) 1 else 0)
      grouped.filter(_._1 != 'J').view.mapValues(_.size) match {
        case m if (m.values.maxOption.getOrElse(0) + jokerCount) == 5 =>
          HandType.FiveOfAKind
        case m if (m.values.max + jokerCount) == 4 => HandType.FourOfAKind
        case m if differentCards == 2 && (m.values.max + jokerCount) == 3 =>
          HandType.FullHouse
        case m if (m.values.max + jokerCount) == 3 => HandType.ThreeOfAKind
        case m if m.size == 3 && m.values.max == 2 => HandType.TwoPairs
        case m if (m.values.max + jokerCount) == 2 => HandType.OnePair
        case _                                     => HandType.HighCard
      }
    }
  }

  def part2(lines: List[String]): Int = solve(
    lines
      .map { line =>
        val hand = HandPart2(line.split(" ")(0).toList)
        val bid = line.split(" ")(1).toInt
        (hand, bid)
      }
  )
}
