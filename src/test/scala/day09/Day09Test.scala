package fr.gplassard.adventofcode
package day09

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day09Test extends AnyWordSpec with Matchers {

  "solve serie" should {

    "handle the base case" in {
      Day09.solveSerie(List(0,0,0,0,0)) should equal(0)
    }

    "handle the recursive case" in {
      Day09.solveSerie(List(1,1,1,1,1,1)) should equal(1)
    }

    "handle the recursive case (part 2)" in {
      Day09.solveSerie(List(2,3,4,5,6)) should equal(7)
    }
  }

  "part1" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day09/sample.txt")).toScala(List)
      Day09.part1(measures) should equal(114)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day09/input.txt")).toScala(List)
      Day09.part1(measures) should equal(1647269739)
    }
  }

  "solve serie part 2" should {

    "handle the base case" in {
      Day09.solveSeriePart2(List(0,0,0,0,0)) should equal(0)
    }

    "handle the recursive case" in {
      Day09.solveSeriePart2(List(2,2,2)) should equal(2)
    }

    "handle the recursive case (part 2)" in {
      Day09.solveSeriePart2(List(0,2,4,6)) should equal(-2)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day09/sample.txt")).toScala(List)
      Day09.part2(measures) should equal(2)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day09/input.txt")).toScala(List)
      Day09.part2(measures) should equal(864)
    }
  }
}
