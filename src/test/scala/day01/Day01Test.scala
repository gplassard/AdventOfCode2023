package fr.gplassard.adventofcode
package day01

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.*
import matchers.should.*

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters._

class Day01Test extends AnyWordSpec with Matchers {

  "part1" should {
    "be 0 for no lines" in {
      Day01.part1(List.empty) should equal(0)
    }

    "work for a single line" in {
      Day01.part1(List("12")) should equal(12)
    }

    "work with random chars in a single line" in {
      Day01.part1(List("aa114554231322aaaa")) should equal(12)
    }

    "work with single char single line" in {
      Day01.part1(List("1")) should equal(11)
    }

    "work with multiple lines" in {
      Day01.part1(List("11", "12")) should equal(23)
    }

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day01/sample.txt"))
        .toScala(List)
      Day01.part1(measures) should equal(142)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day01/input.txt"))
        .toScala(List)
      Day01.part1(measures) should equal(55621)
    }
  }

  "part2" should {
    "be 0 for no lines" in {
      Day01.part2(List.empty) should equal(0)
    }

    "work for a single line" in {
      Day01.part2(List("onetwo")) should equal(12)
    }

    "work with random chars in a single line" in {
      Day01.part2(List("xsqdssqdqsonekjqhsdkjshqdtwofdsfds")) should equal(12)
    }

    "work with single char single line" in {
      Day01.part2(List("one")) should equal(11)
    }

    "work with multiple lines" in {
      Day01.part2(List("oneone", "onetwo")) should equal(23)
    }

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day01/sample-2.txt"))
        .toScala(List)
      Day01.part2(measures) should equal(281)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day01/input.txt"))
        .toScala(List)
      Day01.part2(measures) should equal(53592)
    }
  }

}
