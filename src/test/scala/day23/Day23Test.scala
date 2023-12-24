package fr.gplassard.adventofcode
package day23

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day23Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day23/sample.txt")).toScala(List)
      Day23.part1(measures) should equal(94)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day23/input.txt")).toScala(List)
      Day23.part1(measures) should equal(2186)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day23/sample.txt")).toScala(List)
      Day23.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day23/input.txt")).toScala(List)
      Day23.part2(measures) should equal(-1)
    }
  }
}
