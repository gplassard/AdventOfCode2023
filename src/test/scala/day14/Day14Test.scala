package fr.gplassard.adventofcode
package day14

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day14Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day14/sample.txt")).toScala(List)
      Day14.part1(measures) should equal(136)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day14/input.txt")).toScala(List)
      Day14.part1(measures) should equal(108614)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day14/sample.txt")).toScala(List)
      Day14.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day14/input.txt")).toScala(List)
      Day14.part2(measures) should equal(-1)
    }
  }
}
