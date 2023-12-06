package fr.gplassard.adventofcode
package day5

import scala.collection.mutable.Stack as MStack

object Day5 {

  object Transformer {
    def parse(line: String): Transformer =
      val parts = line.split(" ")
      Transformer(parts(0).toLong, parts(1).toLong, parts(2).toLong)
  }
  case class Transformer(destination: Long, source: Long, length: Long) {
    def transform(value: Long): Option[Long] = {
      if (value > source && value - source < length) {
        Some(destination + (value - source))
      } else {
        None
      }
    }
  }

  def part1(lines: List[String]): Long =
    var numbers = lines(0).replace("seeds:", "").split(" ").map(_.trim).filter(_.nonEmpty).map(_.toLong)
    var transformers = List.empty[Transformer]
    for (line <- lines) {
      if (line.isBlank) {
        numbers = numbers.map(n => transformers.map(t => t.transform(n)).find(_.isDefined).map(_.getOrElse(n)).getOrElse(n))
        transformers = List.empty
      }
      else if (!line.contains(":")) {
        transformers = transformers :+ Transformer.parse(line)
      }
    }
    numbers = numbers.map(n => transformers.map(t => t.transform(n)).find(_.isDefined).map(_.getOrElse(n)).getOrElse(n))
    numbers.minOption.getOrElse(-1)

  case class Range(start: Long, end: Long) {
    def isValid(): Boolean = start <= end
    def transform(transformer: Transformer): (Option[Range], Option[Range], Option[Range]) = {
      val tStart = transformer.source
      val tEnd = transformer.source + transformer.length
      val delta = transformer.destination - transformer.source
      val intersectionRange = Some(Range(Math.max(start, tStart), Math.min(end, tEnd))).filter(_.isValid())
      val belowRange = Some(Range(start, Math.min(end, tStart - 1))).filter(_.isValid())
      val overRange = Some(Range(Math.max(start, tEnd + 1), end)).filter(_.isValid())
      (
        intersectionRange.map(r => Range(r.start  + delta, r.end + delta)),
        belowRange,
        overRange
      )
    }
  }

  def part2(lines: List[String]): Long =
    val numbers = lines(0).replace("seeds:", "").split(" ").map(_.trim).filter(_.nonEmpty).map(_.toLong)
    var seeds = MStack.from(numbers.grouped(2).map{case Array(start, length) => Range(start, start + length - 1)}.toList)
    var transformers = List.empty[Transformer]
    for (line <- (lines appended "")) {
      if (line.isBlank && transformers.nonEmpty) {
        val newSeeds = MStack.empty[Range]
        while (seeds.nonEmpty) {
          val seed = seeds.pop()
          var added = false
          for (transformer <- transformers) {
            val (transformed, lower, higher) = seed.transform(transformer)
            if (transformed.nonEmpty) {
              transformed.foreach({ transformed =>
                added = true
                newSeeds.append(transformed)
              })
              lower.foreach(seeds.append)
              higher.foreach(seeds.append)
            }
          }
          if (!added) {
            newSeeds.append(seed)
          }
        }
        seeds = newSeeds
        transformers = List.empty
      }
      else if (!line.contains(":") && !line.contains("seeds") && !line.isBlank) {
        transformers = transformers :+ Transformer.parse(line)
      }
    }
    seeds.map(_.start).minOption.getOrElse(-1)
}
