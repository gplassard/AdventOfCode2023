package fr.gplassard.adventofcode
package day13

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day13Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/sample.txt"))
        .toScala(List)
      Day13.part1(measures) should equal(5)
    }

    "work for the sample-2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/sample-2.txt"))
        .toScala(List)
      Day13.part1(measures) should equal(400)
    }

    "work for the sample-3" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/sample-3.txt"))
        .toScala(List)
      Day13.part1(measures) should equal(800)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/input.txt"))
        .toScala(List)
      Day13.part1(measures) should equal(30705)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/sample.txt"))
        .toScala(List)
      Day13.part2(measures) should equal(300)
    }
    "work for the sample-2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/sample-2.txt"))
        .toScala(List)
      Day13.part2(measures) should equal(100)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day13/input.txt"))
        .toScala(List)
      Day13.part2(measures) should equal(44615)
    }
  }
}
