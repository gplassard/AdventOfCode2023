package fr.gplassard.adventofcode
package day22

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day22Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day22/sample.txt")).toScala(List)
      Day22.part1(measures) should equal(5)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day22/input.txt")).toScala(List)
      Day22.part1(measures) should equal(405)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day22/sample.txt")).toScala(List)
      Day22.part2(measures) should equal(7)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day22/input.txt")).toScala(List)
      Day22.part2(measures) should equal(-1)
    }
  }
}
