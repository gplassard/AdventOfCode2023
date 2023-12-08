package fr.gplassard.adventofcode
package day04

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day04Test extends AnyWordSpec with Matchers {

  "part1" should {
    "be 0 for no lines" in {
      Day04.part1(List.empty) should equal(0)
    }

    "count if there are winning cards" in {
      Day04.part1(List("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")) should equal(8)
    }

    "be 0 if there are no winning cards" in {
      Day04.part1(List("Card 1: 1 2 3 | 4 5 6")) should equal(0)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day04/sample.txt")).toScala(List)
      Day04.part1(measures) should equal(13)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day04/input.txt")).toScala(List)
      Day04.part1(measures) should equal(27845)
    }
  }

  "part2" should {
    "be 0 for no lines" in {
      Day04.part2(List.empty) should equal(0)
    }

    "count if there are winning cards" in {
      Day04.part2(List(
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
        "Card 2: 41 | 42",
        "Card 3: 41 | 42",
        "Card 4: 41 | 42",
        "Card 5: 41 | 42",
      )) should equal(9)
    }

    "be the number of cards if there are no winning cards" in {
      Day04.part2(List("Card 1: 1 2 3 | 4 5 6")) should equal(1)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day04/sample.txt")).toScala(List)
      Day04.part2(measures) should equal(30)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day04/input.txt")).toScala(List)
      Day04.part2(measures) should equal(9496801)
    }
  }
}
