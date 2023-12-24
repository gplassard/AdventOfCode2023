package fr.gplassard.adventofcode
package day23

import scala.collection.mutable.{Queue => MQueue, Map => MMap, Set => MSet}

object Day23 {
  def part1(lines: List[String]): Int = {
    val grid = lines.map(_.toCharArray.toList)
    val start = (0, lines.head.indexWhere(_ == '.'))
    val end = (lines.length - 1, lines.last.indexWhere(_ == '.'))
    longestPath(start, end, grid, true)
  }

  def longestPath(from: (Int, Int), to: (Int, Int), grid: List[List[Char]], isV1: Boolean): Int = {
    val queue = MQueue((from, 0, Set.empty[(Int, Int)]))
    var maxDistance = -1
    while (queue.nonEmpty) {
      val (position, distance, visited) = queue.dequeue()
      if (position == to) {
        maxDistance = Math.max(maxDistance, distance)
      } else {
        val neighbors = findNeighbors(position, grid, isV1).filterNot(visited.contains)
        neighbors.foreach(n => queue.enqueue((n, distance + 1, visited + position)))
      }
    }
    maxDistance
  }

  def findNeighbors(point: (Int, Int), grid: List[List[Char]], isV1: Boolean): List[(Int, Int)] =
    val base = if (isV1) grid(point._1)(point._2) match {
      case '>' => List((0, 1))
      case '<' => List((0, -1))
      case '^' => List((-1, 0))
      case 'v' => List((1, 0))
      case _ =>  List((0, 1),(0, -1),(1, 0), (-1, 0))
    } else List((0, 1),(0, -1),(1, 0), (-1, 0))
    base
      .map(delta => (point._1 + delta._1, point._2 + delta._2))
      .filter(p => p._1 >= 0 && p._1 < grid.length && p._2 >= 0 && p._2 < grid.head.length)
      .filter(p => grid(p._1)(p._2) != '#')

  def part2(lines: List[String]): Int = {
    val grid = lines.map(_.toCharArray.toList)
    val start = (0, lines.head.indexWhere(_ == '.'))
    val end = (lines.length - 1, lines.last.indexWhere(_ == '.'))
    val contracted = contract(grid, start, end, false)
    dfs(start, end, contracted, Set.empty)
  }

  def contract(grid: List[List[Char]], start: (Int, Int), end: (Int, Int), isV1: Boolean): MMap[(Int, Int), MMap[(Int, Int), Int]] = {
    val pois = findPois(grid, isV1) + start + end
    val distances = MMap.from(pois.map(poi => poi -> MMap.empty[(Int, Int), Int]))
    for {
      poi <- pois
    } {
      val todos = MQueue((poi, 0))
      val seen = MSet(poi)
      while (todos.nonEmpty) {
        val (todo, distance) = todos.dequeue()
        if (distance != 0 && pois.contains(todo)) {
          distances(poi)(todo) = distance
        }
        else {
          val neighbors = findNeighbors(todo, grid, isV1).filterNot(seen.contains)
          for {
            neighbor <- neighbors
          } {
            todos.enqueue((neighbor, distance + 1))
            seen.add(neighbor)
          }
        }
      }
    }
    distances
  }

  def findPois(grid: List[List[Char]], isV1: Boolean): Set[(Int, Int)] = {
    (for {
      x <- grid.indices
      y <- grid.head.indices
      if grid(x)(y) != '#'
      if findNeighbors((x, y), grid, isV1).size >= 3
    } yield (x, y)).toSet
  }

  def dfs(from: (Int, Int), to: (Int, Int), distances: MMap[(Int, Int), MMap[(Int, Int), Int]], visited: Set[(Int, Int)]): Int = {
    if (from == to) {
      0
    }
    else {
      val nexts = distances(from).filterNot(map => visited.contains(map._1))
      nexts.map(n => dfs(n._1, to, distances, visited + from) + n._2).maxOption.getOrElse(Int.MinValue)
    }
  }
}
