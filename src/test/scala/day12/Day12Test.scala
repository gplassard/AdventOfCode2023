package fr.gplassard.adventofcode
package day12

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day12Test extends AnyWordSpec with Matchers {

  "record" should {
    "detect valid" in {
      Day12.Record("#.#.###".toList, List(1,1,3)).isValid() should equal(true)
    }
    "detect invalid" in {
      Day12.Record("#.#.###".toList, List(1, 2, 3)).isValid() should equal(false)
    }
  }

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day12/sample.txt")).toScala(List)
      Day12.part1(measures) should equal(6)
    }

    "work for the sample 2" in {
      val measures = Files.lines(Paths.get("src/test/resources/day12/sample-2.txt")).toScala(List)
      Day12.part1(measures) should equal(21)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day12/input.txt")).toScala(List)
      Day12.part1(measures) should equal(6827)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day12/sample.txt")).toScala(List)
      Day12.part2(measures) should equal(-1)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day12/input.txt")).toScala(List)
      Day12.part2(measures) should equal(-1)
    }
  }
}
