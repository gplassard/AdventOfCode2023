package fr.gplassard.adventofcode
package day2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.*
import matchers.should.*

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters._

class Day2Test extends AnyWordSpec with Matchers {

  "part1" should {
    "be 0 for no lines" in {
      Day2.part1(List.empty) should equal(0)
    }

    "accept a valid game" in {
      Day2.part1(List("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")) should equal(1)
    }

    "reject an invalid game" in {
      Day2.part1(List("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")) should equal(0)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day2/sample.txt")).toScala(List)
      Day2.part1(measures) should equal(8)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day2/input.txt")).toScala(List)
      Day2.part1(measures) should equal(2685)
    }
  }

}
