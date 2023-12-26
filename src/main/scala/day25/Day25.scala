package fr.gplassard.adventofcode
package day25

import org.jgrapht.alg.StoerWagnerMinimumCut
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.collection.mutable.Set as MSet

object Day25 {
  def part1(lines: List[String]): Int = {
    // MinimumSTCutAlgorithm
    val g = new DefaultUndirectedGraph(classOf[DefaultEdge])
    val vertices = MSet.empty[String]
    lines.foreach(line => {
      val source = line.split(":")(0)
      val dests = line.split(":")(1).split(" ").map(_.trim).filter(_.nonEmpty)
      dests.foreach { dest =>
        if (!vertices.contains(source)) {
          vertices.add(source)
          g.addVertex(source)
        }
        if (!vertices.contains(dest)) {
          vertices.add(dest)
          g.addVertex(dest)
        }
        g.addEdge(source, dest)
      }
    })
    val cut = new StoerWagnerMinimumCut(g)
    assert(cut.minCutWeight().toInt == 3, "Min cut hasn't size 3")
    cut.minCut().size() * (vertices.size - cut.minCut().size())
  }

  def part2(lines: List[String]): Int = {
    -1
  }
}
