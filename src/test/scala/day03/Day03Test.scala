package fr.gplassard.adventofcode
package day03

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day03Test extends AnyWordSpec with Matchers {

  "part1" should {
    "be 0 for no lines" in {
      Day03.part1(List.empty) should equal(0)
    }

    "count if there are parts" in {
      Day03.part1(List("467+.114+.")) should equal(581)
    }

    "not count if there are no parts" in {
      Day03.part1(List("467..114..")) should equal(0)
    }

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day03/sample.txt"))
        .toScala(List)
      Day03.part1(measures) should equal(4361)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day03/input.txt"))
        .toScala(List)
      Day03.part1(measures) should equal(531561)
    }
  }

  "part2" should {
    "be 0 for no lines" in {
      Day03.part2(List.empty) should equal(0)
    }

    "count if there is a gear" in {
      Day03.part2(List("10*11...13")) should equal(110)
    }

    "not count if there are no gears" in {
      Day03.part2(List("467*.114..")) should equal(0)
    }

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day03/sample.txt"))
        .toScala(List)
      Day03.part2(measures) should equal(467835)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day03/input.txt"))
        .toScala(List)
      Day03.part2(measures) should equal(83279367)
    }
  }

}
