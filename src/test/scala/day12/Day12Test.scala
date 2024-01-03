package fr.gplassard.adventofcode
package day12

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

class Day12Test extends AnyWordSpec with Matchers {

  "record" should {
    "count both empty" in {
      Day12.Record(List.empty, List.empty).count() should equal(0)
    }
    "count remaining errors counts" in {
      Day12.Record(List.empty, List(1)).count() should equal(0)
    }
    "count remaining errors" in {
      Day12.Record("####".toList, List(0)).count() should equal(0)
    }
    "count remaining solution is valid" in {
      Day12.Record("........".toList, List.empty).count() should equal(1)
    }
    "count remaining solution is valid with errors" in {
      Day12.Record("##..#..###..".toList, List(2, 1, 3)).count() should equal(1)
    }
    "count remaining solution with unknown" in {
      Day12.Record("?#..#..###..".toList, List(2, 1, 3)).count() should equal(1)
    }
    "count remaining solution with more unknown" in {
      Day12.Record("?#?.#..###..".toList, List(2, 1, 3)).count() should equal(2)
    }
    "count from sample" in {
      Day12.Record("#.#.###".toList, List(1, 1, 3)).count() should equal(1)
    }
    "count from sample - 2.1" in {
      Day12.Record("???.###".toList, List(1, 1, 3)).count() should equal(1)
    }
    "count from sample - 2.2" in {
      Day12.Record(".??..??...?##.".toList, List(1, 1, 3)).count() should equal(
        4
      )
    }
    "count from sample - 2.3" in {
      Day12
        .Record("?#?#?#?#?#?#?#?".toList, List(1, 3, 1, 6))
        .count() should equal(1)
    }
    "count from sample - 2.4" in {
      Day12.Record("????.#...#...".toList, List(4, 1, 1)).count() should equal(
        1
      )
    }
    "count from sample - 2.5" in {
      Day12
        .Record("????.######..#####.".toList, List(1, 6, 5))
        .count() should equal(4)
    }
    "count from sample - 2.6" in {
      Day12.Record("?###????????".toList, List(3, 2, 1)).count() should equal(
        10
      )
    }
    "count from sample - 2.6 - sub 1" in {
      Day12.Record(".??????".toList, List(2, 1)).count() should equal(6)
    }
    "count from sample - 2.6 - sub 2" in {
      Day12.Record(".?????".toList, List(2, 1)).count() should equal(3)
    }
    "count from sample - 2.6 - sub 3" in {
      Day12.Record(".????".toList, List(2, 1)).count() should equal(1)
    }
    "count from sample - 2.6 - sub 4" in {
      Day12.Record(".???".toList, List(2, 1)).count() should equal(0)
    }
    "count from sample - 2.6 - sub 5" in {
      Day12.Record("#?".toList, List(2, 1)).count() should equal(0)
    }
  }

  "part1" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day12/sample.txt"))
        .toScala(List)
      Day12.part1(measures) should equal(6)
    }

    "work for the sample 2" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day12/sample-2.txt"))
        .toScala(List)
      Day12.part1(measures) should equal(21)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day12/input.txt"))
        .toScala(List)
      Day12.part1(measures) should equal(6827)
    }
  }

  "part2" should {

    "work for the sample" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day12/sample-2.txt"))
        .toScala(List)
      Day12.part2(measures) should equal(525152)
    }

    "work for the sample - part 1" in {
      Day12.part2(List("???.### 1,1,3")) should equal(1)
    }

    "work for the sample - part 2" in {
      Day12.part2(List(".??..??...?##. 1,1,3")) should equal(16384)
    }

    "work for the sample - part 3" in {
      Day12.part2(List("?#?#?#?#?#?#?#? 1,3,1,6")) should equal(1)
    }

    "work for the sample - part 4" in {
      Day12.part2(List("????.#...#... 4,1,1")) should equal(16)
    }

    "work for the sample - part 5" in {
      Day12.part2(List("????.######..#####. 1,6,5")) should equal(2500)
    }

    "work for the sample - part 6" in {
      Day12.part2(List("?###???????? 3,2,1")) should equal(506250)
    }

    "work for the input" in {
      val measures = Files
        .lines(Paths.get("src/test/resources/day12/input.txt"))
        .toScala(List)
      Day12.part2(measures) should equal(BigInt(1537505634471L))
    }
  }
}
