package fr.gplassard.adventofcode
package day10

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day10Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/sample.txt"))
        .toScala(List)
      Day10.part1(measures) should equal(4)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/input.txt"))
        .toScala(List)
      Day10.part1(measures) should equal(6870)
    }
  }

  "part2" should {

    "work for the sample 2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/sample-2.txt"))
        .toScala(List)
      Day10.part2(measures) should equal(4)
    }

    "work for the sample 3" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/sample-3.txt"))
        .toScala(List)
      Day10.part2(measures) should equal(8)
    }

    "work for the sample 4" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/sample-4.txt"))
        .toScala(List)
      Day10.part2(measures) should equal(10)
    }

    "work for the sample 5" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/sample-5.txt"))
        .toScala(List)
      Day10.part2(measures) should equal(10)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day10/input.txt"))
        .toScala(List)
      Day10.part2(measures) should equal(287)
    }
  }
}
