package fr.gplassard.adventofcode
package day12

import scala.collection.mutable.{Map => MMap}

object Day12 {
  object Record {
    def parse(line: String): Record = Record(
      line.split(" ")(0).toList,
      line.split(" ")(1).split(",").map(_.toInt).toList
    )
  }
  case class Record(springs: List[Char], damagedRecords: List[Int]) {
    def count(cache: MMap[Record, BigInt] = MMap.empty): BigInt = {
      if (cache.contains(this)) return cache(this)
      if (damagedRecords.isEmpty && springs.isEmpty) return 0
      if (damagedRecords.isEmpty) return if (springs.contains('#')) 0 else 1
      if (springs.isEmpty && damagedRecords.nonEmpty) return 0
      if (springs.head == '.')
        return this
          .copy(springs = this.springs.dropWhile(_ == '.'))
          .count(cache)
      if (springs.head == '#') {
        val damagedCount = damagedRecords(0)
        val nextDamaged = springs
          .take(damagedCount)
          .filter(
            _ != '.'
          ) // same as transforming all possible unknown into damaged
        if (nextDamaged.size != damagedCount) {
          return 0
        }
        if (damagedCount == springs.size) {
          return if (damagedRecords.size == 1) 1 else 0
        }
        val nextSprings = springs.drop(damagedCount)
        if (nextSprings.head == '#') {
          return 0
        }
        return this
          .copy(
            springs = nextSprings.updated(0, '.'),
            damagedRecords = this.damagedRecords.drop(1)
          )
          .count(cache)
      }
      val left = this.copy(springs = springs.updated(0, '.'))
      val right = this.copy(springs = springs.updated(0, '#'))
      val leftCount = left.count(cache)
      cache(left) = leftCount
      val rightCount = right.count(cache)
      cache(right) = rightCount
      val sum = leftCount + rightCount
      cache(this) = sum
      sum
    }
  }

  def part1(lines: List[String]): BigInt = {
    val records = lines.map(Record.parse)
    records.map(_.count()).sum
  }

  def part2(lines: List[String]): BigInt = {
    val records = lines
      .map(Record.parse)
      .map(r =>
        r.copy(
          springs = r.springs ++ List('?') ++ r.springs ++ List(
            '?'
          ) ++ r.springs ++ List('?') ++ r.springs ++ List('?') ++ r.springs,
          damagedRecords =
            r.damagedRecords ++ r.damagedRecords ++ r.damagedRecords ++ r.damagedRecords ++ r.damagedRecords
        )
      )

    records.map(_.count()).sum
  }
}
