package fr.gplassard.adventofcode
package day11

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day11Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day11/sample.txt"))
        .toScala(List)
      Day11.part1(measures) should equal(374)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day11/input.txt"))
        .toScala(List)
      Day11.part1(measures) should equal(9605127)
    }
  }

  "solve" should {

    "work for the sample with multiplier 10" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day11/sample.txt"))
        .toScala(List)
      Day11.solve(measures, BigInt(10)) should equal(1030)
    }

    "work for the sample with multiplier 100" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day11/sample.txt"))
        .toScala(List)
      Day11.solve(measures, BigInt(100)) should equal(8410)
    }
  }

  "part2" should {

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day11/input.txt"))
        .toScala(List)
      Day11.part2(measures) should equal(BigInt(458191688761L))
    }
  }
}
