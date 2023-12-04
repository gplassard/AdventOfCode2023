package fr.gplassard.adventofcode
package day4

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day4Test extends AnyWordSpec with Matchers {

  "part1" should {
    "be 0 for no lines" in {
      Day4.part1(List.empty) should equal(0)
    }

    "count if there are winning cards" in {
      Day4.part1(List("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")) should equal(8)
    }

    "be 0 if there are no winning cards" in {
      Day4.part1(List("Card 1: 1 2 3 | 4 5 6")) should equal(0)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day4/sample.txt")).toScala(List)
      Day4.part1(measures) should equal(13)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day4/input.txt")).toScala(List)
      Day4.part1(measures) should equal(27845)
    }
  }
}