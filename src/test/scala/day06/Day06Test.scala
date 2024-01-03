package fr.gplassard.adventofcode
package day06

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day06Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day06/sample.txt"))
        .toScala(List)
      Day06.part1(measures) should equal(288)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day06/input.txt"))
        .toScala(List)
      Day06.part1(measures) should equal(800280)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day06/sample.txt"))
        .toScala(List)
      Day06.part2(measures) should equal(71503)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day06/input.txt"))
        .toScala(List)
      Day06.part2(measures) should equal(45128024)
    }
  }
}
