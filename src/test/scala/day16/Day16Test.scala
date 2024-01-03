package fr.gplassard.adventofcode
package day16

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day16Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day16/sample.txt"))
        .toScala(List)
      Day16.part1(measures) should equal(46)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day16/input.txt"))
        .toScala(List)
      Day16.part1(measures) should equal(7034)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day16/sample.txt"))
        .toScala(List)
      Day16.part2(measures) should equal(51)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day16/input.txt"))
        .toScala(List)
      Day16.part2(measures) should equal(7759)
    }
  }
}
