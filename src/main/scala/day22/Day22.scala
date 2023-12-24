package fr.gplassard.adventofcode
package day22

import scala.collection.mutable.{Map => MMap, Set => MSet}

object Day22 {
  object Brick {
    def parse(line: String): Brick = {
      val Array(x1, y1, z1, x2, y2, z2) = line.replace("~", ",").split(",").map(_.trim.toInt)
      Brick(x1, y1, z1, x2, y2, z2)
    }
  }
  case class Brick(x1: Int, y1: Int, var z1: Int, x2: Int, y2: Int, var z2: Int) {
    def intersect(other: Brick): Boolean = Math.max(x1, other.x1) <= Math.min(x2, other.x2)
        && Math.max(y1, other.y1) <= Math.min(y2, other.y2)
  }

  def part1(lines: List[String]): Int = {
    val bricks = fall(lines.map(Brick.parse))
    val (supports, isSupportedBy) = buildSupportRelations(bricks)
    count(bricks, supports, isSupportedBy)
  }

  def fall(bricks: List[Brick]): List[Brick] = {
    val sorted = bricks.sortBy(_.z1)
    for {
      (brick, index) <- sorted.zipWithIndex
    } {
      var height = 1
      for {
        under <- sorted.take(index)
      } {
        if (under.intersect(brick)) {
          height = Math.max(height, under.z2 + 1)
        }
      }
      brick.z2 = height + (brick.z2 - brick.z1)
      brick.z1 = height
    }
    bricks.sortBy(_.z1)
  }

  def buildSupportRelations(bricks: List[Brick]): (MMap[Int, MSet[Int]], MMap[Int, MSet[Int]]) = {
    val supports = MMap.from(bricks.indices.map(i => i -> MSet.empty[Int]))
    val isSupportedBy = MMap.from(bricks.indices.map(i => i -> MSet.empty[Int]))
    for {
      (top, indexT) <- bricks.zipWithIndex
      (bottom, indexB) <- bricks.take(indexT).zipWithIndex
      if (top.intersect(bottom))
      if (top.z1 == bottom.z2 + 1)
    } {
      supports(indexB) += indexT
      isSupportedBy(indexT) += indexB
    }
    (supports, isSupportedBy)
  }

  def count(bricks: List[Brick], supports: MMap[Int, MSet[Int]], isSupportedBy: MMap[Int, MSet[Int]]): Int = {
    bricks.indices.count(i => {
      val supportedByI = supports(i)
      supportedByI.forall(j => isSupportedBy(j).size >= 2)
    })
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
