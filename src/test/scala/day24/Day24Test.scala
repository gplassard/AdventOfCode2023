package fr.gplassard.adventofcode
package day24

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day24Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day24/sample.txt"))
        .toScala(List)
      Day24.part1(measures, true) should equal(2)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day24/input.txt"))
        .toScala(List)
      Day24.part1(measures, false) should equal(19976)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day24/sample.txt"))
        .toScala(List)
      Day24.part2(measures) should equal(47)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day24/input.txt"))
        .toScala(List)
      Day24.part2(measures) should equal(849377770236905L)
    }
  }
}
