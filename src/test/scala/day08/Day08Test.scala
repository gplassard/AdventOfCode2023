package fr.gplassard.adventofcode
package day08

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day08Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day08/sample.txt"))
        .toScala(List)
      Day08.part1(measures) should equal(2)
    }

    "work for the sample 2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day08/sample-2.txt"))
        .toScala(List)
      Day08.part1(measures) should equal(6)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day08/input.txt"))
        .toScala(List)
      Day08.part1(measures) should equal(19637)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day08/sample-3.txt"))
        .toScala(List)
      Day08.part2(measures) should equal(6)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day08/input.txt"))
        .toScala(List)
      Day08.part2(measures) should equal(BigInt("8811050362409"))
    }
  }
}
