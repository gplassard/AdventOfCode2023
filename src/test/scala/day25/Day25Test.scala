package fr.gplassard.adventofcode
package day25

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day25Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day25/sample.txt")).toScala(List)
      Day25.part1(measures) should equal(54)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day25/input.txt")).toScala(List)
      Day25.part1(measures) should equal(567606)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day25/sample.txt")).toScala(List)
      Day25.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day25/input.txt")).toScala(List)
      Day25.part2(measures) should equal(-1)
    }
  }
}
