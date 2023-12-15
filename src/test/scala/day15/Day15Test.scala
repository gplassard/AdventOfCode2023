package fr.gplassard.adventofcode
package day15

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day15Test extends AnyWordSpec with Matchers {

  "hash" should {
    "compute the hash of HASH" in {
      Day15.hash("HASH") should equal(52)
    }
  }

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day15/sample.txt")).toScala(List)
      Day15.part1(measures) should equal(1320)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day15/input.txt")).toScala(List)
      Day15.part1(measures) should equal(516070)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day15/sample.txt")).toScala(List)
      Day15.part2(measures) should equal(145)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day15/input.txt")).toScala(List)
      Day15.part2(measures) should equal(244981)
    }
  }
}
