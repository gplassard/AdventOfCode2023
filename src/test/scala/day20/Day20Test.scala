package fr.gplassard.adventofcode
package day20

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day20Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day20/sample.txt")).toScala(List)
      Day20.part1(measures) should equal(32000000)
    }

    "work for the sample-2" in {
      val measures = Files.lines(Paths.get("src/test/resources/day20/sample-2.txt")).toScala(List)
      Day20.part1(measures) should equal(11687500)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day20/input.txt")).toScala(List)
      Day20.part1(measures) should equal(806332748)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day20/sample.txt")).toScala(List)
      Day20.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day20/input.txt")).toScala(List)
      Day20.part2(measures) should equal(-1)
    }
  }
}
