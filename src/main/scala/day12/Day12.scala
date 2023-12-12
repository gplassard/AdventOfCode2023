package fr.gplassard.adventofcode
package day12

import scala.collection.mutable.{ListBuffer => MList}

object Day12 {
  object Record {
    def parse(line: String): Record = Record(line.split(" ")(0).toList, line.split(" ")(1).split(",").map(_.toInt).toList)
  }
  case class Record(springs: List[Char], damagedRecords: List[Int]) {
    def isValid(): Boolean = {
      val counters = MList.empty[Int]
      var counter = 0
      for {spring <- springs} {
        if (spring == '#') {
          counter = counter + 1
        }
        else if (counter > 0) {
          counters.addOne(counter)
          counter = 0
        }
      }
      if (counter > 0) {
        counters.addOne(counter)
      }
      counters.toList == damagedRecords
    }

    def generate(index: Int): LazyList[Record] = {
      if (index == springs.length) return LazyList(this)
      if (springs(index) != '?') return generate(index + 1)
      val left = this.copy(springs = this.springs.updated(index, '#')).generate(index + 1)
      val right = this.copy(springs = this.springs.updated(index, '.')).generate(index + 1)
      left ++ right
    }
  }

  def part1(lines: List[String]): Int = {
    val records = lines.map(Record.parse)
    records.flatMap(_.generate(0).filter(_.isValid())).size
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
