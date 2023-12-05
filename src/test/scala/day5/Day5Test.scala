package fr.gplassard.adventofcode
package day5

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day5Test extends AnyWordSpec with Matchers {


  "part1" should {

    "be the lowest seed if no transformation" in {
      Day5.part1(List("seeds: 79 14 55 13")) should equal(13)
    }

    "be capable of 1 noop transformation" in {
      Day5.part1(List(
        "seeds: 79 14 55 13",
        "",
        "seed-to-soil map:",
        "50 98 2",
        "52 50 48",
      )) should equal(13)
    }

    "be capable of 1 real transformation" in {
      Day5.part1(List(
        "seeds: 79 55",
        "",
        "seed-to-soil map:",
        "50 98 2",
        "52 50 48",
      )) should equal(57)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day5/sample.txt")).toScala(List)
      Day5.part1(measures) should equal(35)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day5/input.txt")).toScala(List)
      Day5.part1(measures) should equal(525792406)
    }
  }
}
