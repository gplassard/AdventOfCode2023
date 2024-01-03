package fr.gplassard.adventofcode
package day01

object Day01 {

  def part1(lines: List[String]): Int =
    lines
      .map(word =>
        val digits = word.filter(c => c.isDigit)
        Integer.parseInt(s"${digits.head}${digits.last}")
      )
      .sum

  private val numberStringToDigit: Map[String, Char] = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9'
  )

  def part2(lines: List[String]): Int = {
    val linesWithDigits = lines
      .map(word =>
        word.zipWithIndex
          .map((char, index) => {
            numberStringToDigit
              .get(word.substring(index, Math.min(index + 3, word.length)))
              .orElse(
                numberStringToDigit.get(
                  word.substring(index, Math.min(index + 4, word.length))
                )
              )
              .orElse(
                numberStringToDigit
                  .get(word.substring(index, Math.min(index + 5, word.length)))
              )
              .getOrElse(char)
          })
          .mkString
      )
    part1(linesWithDigits)
  }
}
