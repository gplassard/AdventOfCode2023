package fr.gplassard.adventofcode
package day18

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day18Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day18/sample.txt"))
        .toScala(List)
      Day18.part1(measures) should equal(62)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day18/input.txt"))
        .toScala(List)
      Day18.part1(measures) should equal(41019)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day18/sample.txt"))
        .toScala(List)
      Day18.part2(measures) should equal(952408144115L)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day18/input.txt"))
        .toScala(List)
      Day18.part2(measures) should equal(96116995735219L)
    }
  }
}
