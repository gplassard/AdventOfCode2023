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
      val measures = Files.lines(Paths.get("src/test/resources/day11/sample.txt")).toScala(List)
      Day11.part1(measures) should equal(374)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day11/input.txt")).toScala(List)
      Day11.part1(measures) should equal(9605127)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day11/sample.txt")).toScala(List)
      Day11.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day11/input.txt")).toScala(List)
      Day11.part2(measures) should equal(-1)
    }
  }
}
