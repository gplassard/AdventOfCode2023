package fr.gplassard.adventofcode
package day15

object Day15 {
  def part1(lines: List[String]): Int =
    lines(0).split(",").map(hash).sum

  def hash(text: String): Int =
    text.foldLeft(0)((value, char) => ((value + char.toInt)  * 17) % 256)

  private type BoxIndex = Int
  private type Label = String
  private type Focal = Int

  def part2(lines: List[String]): Int = {
    var boxes = Map.empty[BoxIndex, List[(Label, Focal)]]
    val instructions = lines(0).split(",")
    for {
      instruction <- instructions
    } {
      if (instruction.contains("=")) {
        val label = instruction.reverse.drop(2).reverse
        val focal = Integer.parseInt(instruction.last.toString)
        boxes = boxes.updatedWith(hash(label)) { previous =>
          val lenses = previous.getOrElse(List.empty)
          val alreadyPresent = lenses.exists(_._1 == label)
          Some(
            if (alreadyPresent) lenses.map(lens => if (lens._1 == label) (lens._1, focal) else lens)
            else lenses :+ (label, focal)
          )
        }
      }
      else if (instruction.contains("-")) {
        val label = instruction.reverse.drop(1).reverse
        boxes = boxes.updatedWith(hash(label))(_.map(_.filter(_._1 != label)))
      }
    }
    boxes
      .toList
      .map((boxIndex, box) => (boxIndex + 1) * box.zipWithIndex.map((lens, lensIndex) => (lensIndex + 1) * lens._2).sum)
      .sum
  }
}
