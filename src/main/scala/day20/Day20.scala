package fr.gplassard.adventofcode
package day20

import day20.Day20.Pulse._
import scala.collection.mutable.{Queue => MQueue, Map => MMap}

object Day20 {
  enum Pulse {
    case LOW, HIGH
  }

  object Module {
    def parse(line: String): Module = {
      val splitted = line.split(" -> ")
      val connected = splitted(1).split(",").map(_.trim).toList
      splitted(0) match {
        case "broadcaster" => Broadcaster(splitted(0), connected)
        case _ if line.startsWith("%") =>
          FlipFlop(splitted(0).drop(1), false, connected)
        case _ if line.startsWith("&") =>
          Conjunction(splitted(0).drop(1), Map.empty, connected)
      }
    }
  }
  trait Module {
    val name: String
    val connectedModules: List[String]
    def push(from: String, pulse: Pulse): (Module, Option[Pulse])
  }
  case class Broadcaster(name: String, connectedModules: List[String])
      extends Module {
    override def push(from: String, pulse: Pulse): (Module, Option[Pulse]) =
      (this, Some(pulse))
  }
  case class FlipFlop(
      name: String,
      isOn: Boolean,
      connectedModules: List[String]
  ) extends Module {
    override def push(from: String, pulse: Pulse): (Module, Option[Pulse]) =
      (pulse, isOn) match {
        case (HIGH, _)  => (this, None)
        case (_, true)  => (this.copy(isOn = false), Some(LOW))
        case (_, false) => (this.copy(isOn = true), Some(HIGH))
      }
  }
  case class Conjunction(
      name: String,
      incoming: Map[String, Pulse],
      connectedModules: List[String]
  ) extends Module {
    override def push(from: String, pulse: Pulse): (Module, Option[Pulse]) =
      val updated = this.copy(incoming = incoming + (from -> pulse))
      if (updated.incoming.values.forall(_ == HIGH)) (updated, Some(LOW))
      else (updated, Some(HIGH))
  }

  case class Config(
      modules: Map[String, Module],
      predecessors: Map[String, Int] = Map.empty
  ) {
    def initConjuctions(): Config = {
      val conjunctions = modules.values
        .filter(_.isInstanceOf[Conjunction])
        .map(_.asInstanceOf[Conjunction])
        .toList
      conjunctions.foldLeft(this)((config, conjunction) => {
        val inputs = modules.values
          .filter(_.connectedModules.contains(conjunction.name))
          .map(_.name)
          .toList
        config.copy(
          modules = config.modules + (conjunction.name -> conjunction
            .copy(incoming = inputs.map(i => i -> LOW).toMap))
        )
      })
    }
    def initPredecessors(): Config = {
      this.copy(predecessors =
        modules.values
          .filter(m => m.connectedModules.contains("xn"))
          .map(m => m.name -> 0)
          .toMap
      )
    }
  }

  def part1(lines: List[String]): Int = {
    var config = Config(lines.map(Module.parse).map(m => m.name -> m).toMap)
      .initConjuctions()
    var lowPulses = 0
    var highPulses = 0
    for (i <- 0 until 1000) {
      val res = push(config)
      config = res._1
      lowPulses += res._2
      highPulses += res._3
    }
    highPulses * lowPulses
  }

  def push(config: Config): (Config, Int, Int) = {
    val pulses = MMap(HIGH -> 0, LOW -> 0)
    val todos =
      MQueue[(String, Pulse, List[String])](("input", LOW, List("broadcaster")))
    var updatedConfig = config
    while (todos.nonEmpty) {
      val (source, signal, modules) = todos.dequeue()
      for (moduleName <- modules) {
        pulses(signal) += 1
        updatedConfig.modules.get(moduleName) match {
          case None => {}
          case Some(module) => {
            val (newModule, pulse) = module.push(source, signal)
            updatedConfig = updatedConfig.copy(modules =
              updatedConfig.modules + (moduleName -> newModule)
            )
            pulse.foreach { pulse =>
              todos.enqueue((moduleName, pulse, newModule.connectedModules))
            }
          }
        }
        if (signal == HIGH && moduleName == "xn") {
          updatedConfig = updatedConfig.copy(predecessors =
            updatedConfig.predecessors.updatedWith(source)(opt =>
              opt.map(_ + 1)
            )
          )
        }
      }
    }
    (updatedConfig, pulses(LOW), pulses(HIGH))
  }

  def part2(lines: List[String]): BigInt = {
    var config = Config(lines.map(Module.parse).map(m => m.name -> m).toMap)
      .initConjuctions()
      .initPredecessors()
    var cycles = Map.from(config.predecessors.keys.map(k => k -> 0))
    var cycle = 0
    while (cycles.values.toList.contains(0)) {
      cycle = cycle + 1
      val res = push(
        config.copy(predecessors = config.predecessors.map((k, _) => (k, 0)))
      )
      config = res._1
      for {
        predecessor <- config.predecessors.filter(_._2 == 1).keys
      } {
        cycles =
          cycles.updatedWith(predecessor)(_.filter(_ != 0).orElse(Some(cycle)))
      }
    }
    lcm(cycles.values.toList.map(BigInt(_)))
  }

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) { (a, b) =>
    b * a / LazyList
      .iterate((a, b)) { case (x, y) => (y, x % y) }
      .dropWhile(_._2 != 0)
      .head
      ._1
      .abs
  }
}
