package fr.gplassard.adventofcode
package day08

object Day08 {
  def part1(lines: List[String]): Int = {
    val instructions = lines(0).toList

    val paths = lines.drop(2).map { line =>
      val src = line.split(" = ")(0)
      val dest = line.split(" = ")(1).replace("(", "").replace(")", "").split(", ")
      val left = dest(0).trim
      val right = dest(1).trim
      src -> (left, right)
    }.toMap

    var current = "AAA"
    var count = 0
    while (current != "ZZZ") {
      val instruction = instructions(count % instructions.size)
      current = if(instruction == 'L') paths(current)._1 else paths(current)._2
      count += 1
    }
    count
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
