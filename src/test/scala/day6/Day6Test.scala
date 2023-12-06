package fr.gplassard.adventofcode
package day6

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day6Test extends AnyWordSpec with Matchers {


  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day6/sample.txt")).toScala(List)
      Day6.part1(measures) should equal(288)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day6/input.txt")).toScala(List)
      Day6.part1(measures) should equal(800280)
    }
  }
}
