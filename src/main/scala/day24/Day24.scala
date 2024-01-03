package fr.gplassard.adventofcode
package day24

import org.matheclipse.core.eval.ExprEvaluator
import org.matheclipse.core.eval.util.SolveUtils
import org.matheclipse.core.reflection.system.Solve

object Day24 {
  object HayStack {
    def parse(line: String): HayStack = {
      val Array(x, y, z, vx, vy, vz) =
        line.replace("@", ",").split(",").map(_.trim).map(BigDecimal.apply)
      HayStack(x, y, z, vx, vy, vz)
    }
  }
  case class HayStack(
      x: BigDecimal,
      y: BigDecimal,
      z: BigDecimal,
      vx: BigDecimal,
      vy: BigDecimal,
      vz: BigDecimal
  ) {
    val a = vy
    val b = -vx
    val c = (vy * x) - (vx * y)
    def isParallel(other: HayStack): Boolean = a * other.b == b * other.a
  }

  def part1(lines: List[String], isSample: Boolean): Int = {
    val hayStacks = lines.map(HayStack.parse)
    var total = 0
    for {
      (h1, i) <- hayStacks.zipWithIndex
      h2 <- hayStacks.take(i)
      if !h1.isParallel(h2)
    } {
      val x = (h1.c * h2.b - h2.c * h1.b) / (h1.a * h2.b - h2.a * h1.b)
      val y = (h2.c * h1.a - h1.c * h2.a) / (h1.a * h2.b - h2.a * h1.b)
      val bounds =
        if (isSample) (7L, 27L) else (200000000000000L, 400000000000000L)
      if (
        bounds(0) <= x && x <= bounds(1) && bounds(0) <= y && y <= bounds(1)
      ) {
        val futureX1 = (x - h1.x) * h1.vx >= 0
        val futureY1 = (y - h1.y) * h1.vy >= 0
        val futureX2 = (x - h2.x) * h2.vx >= 0
        val futureY2 = (y - h2.y) * h2.vy >= 0
        if (futureX1 && futureY1 && futureX2 && futureY2) {
          total += 1
        }
      }
    }
    total
  }

  def part2(lines: List[String]): Long = {
    val hayStacks = lines.map(HayStack.parse)
    val util = new ExprEvaluator()
    val equations = hayStacks
      .take(10)
      .flatMap(h =>
        List(
          s"(x - ${h.x}) * (${h.vy} - vy) - (y - ${h.y}) * (${h.vx} - vx) == 0",
          s"(y - ${h.y}) * (${h.vz} - vz) - (z - ${h.z}) * (${h.vy} - vy) == 0"
        )
      )
    val variables = "x,y,z,vx,vy,vz"
    val res = util.eval(s"Solve({${equations.mkString(",")}},{${variables}})")
    val values = res.toString
      .replace("{{", "")
      .replace("}}", "")
      .replace("\n", "")
      .split(",")
      .map(exp => exp.split("->")(0) -> exp.split("->")(1).toLong)
      .toMap
    values("x") + values("y") + values("z")
  }
}
