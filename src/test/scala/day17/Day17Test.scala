package fr.gplassard.adventofcode
package day17

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day17Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/sample.txt"))
        .toScala(List)
      Day17.part1(measures) should equal(102)
    }

    "work for the sample-2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/sample-2.txt"))
        .toScala(List)
      Day17.part1(measures) should equal(59)
    }

    "work for the sample-3" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/sample-3.txt"))
        .toScala(List)
      Day17.part1(measures) should equal(17)
    }
    "work for the sample-4" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/sample-4.txt"))
        .toScala(List)
      Day17.part1(measures) should equal(2)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/input.txt"))
        .toScala(List)
      Day17.part1(measures) should equal(1039)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/sample.txt"))
        .toScala(List)
      Day17.part2(measures) should equal(94)
    }

    "work for the sample-2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/sample-2.txt"))
        .toScala(List)
      Day17.part2(measures) should equal(71)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day17/input.txt"))
        .toScala(List)
      Day17.part2(measures) should equal(1201)
    }
  }
}
