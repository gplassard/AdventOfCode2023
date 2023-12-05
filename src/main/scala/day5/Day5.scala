package fr.gplassard.adventofcode
package day5

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

}
