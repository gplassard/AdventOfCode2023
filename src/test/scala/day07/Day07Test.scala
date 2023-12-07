package fr.gplassard.adventofcode
package day07

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day07Test extends AnyWordSpec with Matchers {

  "hand" should {
    "AAAAA is five of a kind" in {
      Day07.Hand("AAAAA".toList).handType() should equal(Day07.HandType.FiveOfAKind)
    }
    "AA8AA is four of a kind" in {
      Day07.Hand("AA8AA".toList).handType() should equal(Day07.HandType.FourOfAKind)
    }
    "23332 is full house" in {
      Day07.Hand("23332".toList).handType() should equal(Day07.HandType.FullHouse)
    }
    "TTT98 is three of a kind" in {
      Day07.Hand("TTT98".toList).handType() should equal(Day07.HandType.ThreeOfAKind)
    }
    "23432 is two pair" in {
      Day07.Hand("23432".toList).handType() should equal(Day07.HandType.TwoPairs)
    }
    "A23A4 is one pair" in {
      Day07.Hand("A23A4".toList).handType() should equal(Day07.HandType.OnePair)
    }
    "23456 is high card" in {
      Day07.Hand("23456".toList).handType() should equal(Day07.HandType.HighCard)
    }
  }

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day07/sample.txt")).toScala(List)
      Day07.part1(measures) should equal(6440)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day07/input.txt")).toScala(List)
      Day07.part1(measures) should equal(250898830)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day07/sample.txt")).toScala(List)
      Day07.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day07/input.txt")).toScala(List)
      Day07.part2(measures) should equal(-1)
    }
  }
}
