package fr.gplassard.adventofcode
package day19

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day19Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day19/sample.txt"))
        .toScala(List)
      Day19.part1(measures) should equal(19114)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day19/input.txt"))
        .toScala(List)
      Day19.part1(measures) should equal(489392)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day19/sample.txt"))
        .toScala(List)
      Day19.part2(measures) should equal(167409079868000L)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day19/input.txt"))
        .toScala(List)
      Day19.part2(measures) should equal(134370637448305L)
    }
  }
}
