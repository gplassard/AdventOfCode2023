package fr.gplassard.adventofcode
package day1

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.*
import matchers.should.*

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters._

class Day1Test extends AnyWordSpec with Matchers {

  "part1" should {
    "be 0 for no lines" in {
      Day1.part1(List.empty) should equal(0)
    }

    "work for a single line" in {
      Day1.part1(List("12")) should equal(12)
    }

    "work with random chars in a single line" in {
      Day1.part1(List("aa114554231322aaaa")) should equal(12)
    }

    "work with single char single line" in {
      Day1.part1(List("1")) should equal(11)
    }

    "work with multiple lines" in {
      Day1.part1(List("11", "12")) should equal(23)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day1/sample.txt")).toScala(List)
      Day1.part1(measures) should equal(142)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day1/input.txt")).toScala(List)
      Day1.part1(measures) should equal(55621)
    }
  }

}
