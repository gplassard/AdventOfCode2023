package fr.gplassard.adventofcode
package day07

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day07Test extends AnyWordSpec with Matchers {

  "HandPart1" should {
    "AAAAA is five of a kind" in {
      Day07.HandPart1("AAAAA".toList).handType() should equal(
        Day07.HandType.FiveOfAKind
      )
    }
    "AA8AA is four of a kind" in {
      Day07.HandPart1("AA8AA".toList).handType() should equal(
        Day07.HandType.FourOfAKind
      )
    }
    "23332 is full house" in {
      Day07.HandPart1("23332".toList).handType() should equal(
        Day07.HandType.FullHouse
      )
    }
    "TTT98 is three of a kind" in {
      Day07.HandPart1("TTT98".toList).handType() should equal(
        Day07.HandType.ThreeOfAKind
      )
    }
    "23432 is two pair" in {
      Day07.HandPart1("23432".toList).handType() should equal(
        Day07.HandType.TwoPairs
      )
    }
    "A23A4 is one pair" in {
      Day07.HandPart1("A23A4".toList).handType() should equal(
        Day07.HandType.OnePair
      )
    }
    "23456 is high card" in {
      Day07.HandPart1("23456".toList).handType() should equal(
        Day07.HandType.HighCard
      )
    }
  }

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day07/sample.txt"))
        .toScala(List)
      Day07.part1(measures) should equal(6440)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day07/input.txt"))
        .toScala(List)
      Day07.part1(measures) should equal(250898830)
    }
  }

  "HandPart2" should {
    "AAAAA is five of a kind" in {
      Day07.HandPart2("AAAAA".toList).handType() should equal(
        Day07.HandType.FiveOfAKind
      )
    }
    "AA8AA is four of a kind" in {
      Day07.HandPart2("AA8AA".toList).handType() should equal(
        Day07.HandType.FourOfAKind
      )
    }
    "T55J5 is four of a kind" in {
      Day07.HandPart2("T55J5".toList).handType() should equal(
        Day07.HandType.FourOfAKind
      )
    }
    "QQQJA is four of a kind" in {
      Day07.HandPart2("QQQJA".toList).handType() should equal(
        Day07.HandType.FourOfAKind
      )
    }
    "KTJJT is four of a kind" in {
      Day07.HandPart2("KTJJT".toList).handType() should equal(
        Day07.HandType.FourOfAKind
      )
    }
    "23332 is full house" in {
      Day07.HandPart2("23332".toList).handType() should equal(
        Day07.HandType.FullHouse
      )
    }
    "TTT98 is three of a kind" in {
      Day07.HandPart2("TTT98".toList).handType() should equal(
        Day07.HandType.ThreeOfAKind
      )
    }
    "23432 is two pair" in {
      Day07.HandPart2("23432".toList).handType() should equal(
        Day07.HandType.TwoPairs
      )
    }
    "KK677 is two pair" in {
      Day07.HandPart2("KK677".toList).handType() should equal(
        Day07.HandType.TwoPairs
      )
    }
    "A23A4 is one pair" in {
      Day07.HandPart2("A23A4".toList).handType() should equal(
        Day07.HandType.OnePair
      )
    }
    "23456 is high card" in {
      Day07.HandPart2("23456".toList).handType() should equal(
        Day07.HandType.HighCard
      )
    }
    "FullHouse with 1 joker" in {
      Day07.HandPart2("22J33".toList).handType() should equal(
        Day07.HandType.FullHouse
      )
    }
    "OnePair with 1 joker" in {
      Day07.HandPart2("2J345".toList).handType() should equal(
        Day07.HandType.OnePair
      )
    }
    "FiveOfAKind with 5 joker" in {
      Day07.HandPart2("JJJJJ".toList).handType() should equal(
        Day07.HandType.FiveOfAKind
      )
    }
    "FiveOfAKind with 4 joker" in {
      Day07.HandPart2("JJJJ1".toList).handType() should equal(
        Day07.HandType.FiveOfAKind
      )
    }
    "FourOfAKind with 3 joker" in {
      Day07.HandPart2("JJJ12".toList).handType() should equal(
        Day07.HandType.FourOfAKind
      )
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day07/sample.txt"))
        .toScala(List)
      Day07.part2(measures) should equal(5905)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day07/input.txt"))
        .toScala(List)
      Day07.part2(measures) should equal(252127335)
    }
  }
}
