package fr.gplassard.adventofcode
package day21

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day21Test extends AnyWordSpec with Matchers {

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day21/sample.txt")).toScala(List)
      Day21.part1(measures) should equal(42)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day21/input.txt")).toScala(List)
      Day21.part1(measures) should equal(3758)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day21/sample.txt")).toScala(List)
      Day21.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day21/input.txt")).toScala(List)
      Day21.part2(measures) should equal(-1)
    }
  }
}
