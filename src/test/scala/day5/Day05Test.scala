package fr.gplassard.adventofcode
package day05

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day05Test extends AnyWordSpec with Matchers {


  "part1" should {

    "be the lowest seed if no transformation" in {
      Day05.part1(List("seeds: 79 14 55 13")) should equal(13)
    }

    "be capable of 1 noop transformation" in {
      Day05.part1(List(
        "seeds: 79 14 55 13",
        "",
        "seed-to-soil map:",
        "50 98 2",
        "52 50 48",
      )) should equal(13)
    }

    "be capable of 1 real transformation" in {
      Day05.part1(List(
        "seeds: 79 55",
        "",
        "seed-to-soil map:",
        "50 98 2",
        "52 50 48",
      )) should equal(57)
    }

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day05/sample.txt")).toScala(List)
      Day05.part1(measures) should equal(35)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day05/input.txt")).toScala(List)
      Day05.part1(measures) should equal(525792406)
    }
  }

  "Range" should {
    "not intersect with a transformer (lower)" in {
      // source             0...10
      // transformer from   ...........100...110
      // transformer to     .........................200...210
      // result             0...10

      val (intersected, lower, higher) = Day05.Range(0, 10).transform(Day05.Transformer(200, 100, 10))

      intersected should equal(None)
      lower should equal(Some(Day05.Range(0, 10)))
      higher should equal(None)
    }

    "not intersect with a transformer (higher)" in {
      // source             .....................................300...310
      // transformer from   ...........100...110
      // transformer to     .........................200...210
      // result             .....................................300...310
      val (intersected, lower, higher) = Day05.Range(300, 310).transform(Day05.Transformer(200, 100, 10))

      intersected should equal(None)
      lower should equal(None)
      higher should equal(Some(Day05.Range(300, 310)))
    }

    "intersect in lower" in {
      // source             .........95.....105
      // transformer from   ............100.....110
      // transformer to     .........................200.....210
      // result             .........95.99...........200.205
      val (intersected, lower, higher) = Day05.Range(95, 105).transform(Day05.Transformer(200, 100, 10))

      intersected should equal(Some(Day05.Range(200, 205)))
      lower should equal(Some(Day05.Range(95, 99)))
      higher should equal(None)
    }

    "intersect in higher" in {
      // source             ................105..........115
      // transformer from   ............100.....110
      // transformer to     ......................................200.....210
      // result             ........................111...115.........205.210
      val (intersected, lower, higher) = Day05.Range(105, 115).transform(Day05.Transformer(200, 100, 10))

      intersected should equal(Some(Day05.Range(205, 210)))
      lower should equal(None)
      higher should equal(Some(Day05.Range(111, 115)))
    }

    "overlapping source" in {
      // source             .......95....................115
      // transformer from   ............100.....110
      // transformer to     ...................................200.....210
      // result             .......95.99............111..115...200.....210
      val (intersected, lower, higher) = Day05.Range(95, 115).transform(Day05.Transformer(200, 100, 10))

      intersected should equal(Some(Day05.Range(200, 210)))
      lower should equal(Some(Day05.Range(95, 99)))
      higher should equal(Some(Day05.Range(111, 115)))
    }

    "overlapping transformer" in {
      // source             ............100.....110
      // transformer from   .......95.................115
      // transformer to     ...................................195.............215
      // result             ........................................200..210
      val (intersected, lower, higher) = Day05.Range(100, 110).transform(Day05.Transformer(195, 95, 20))

      intersected should equal(Some(Day05.Range(200, 210)))
      lower should equal(None)
      higher should equal(None)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files.lines(Paths.get("src/test/resources/day05/sample.txt")).toScala(List)
      Day05.part2(measures) should equal(46)
    }

    "work for the input" in {
      val measures = Files.lines(Paths.get("src/test/resources/day05/input.txt")).toScala(List)
      Day05.part2(measures) should equal(79004094)
    }
  }
}
