import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.asScalaIteratorConverter
import scala.util.matching.Regex
import java.nio.file.*
import java.io.IOException
import java.nio.charset.StandardCharsets

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2023",
    idePackagePrefix := Some("fr.gplassard.adventofcode")
  )

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.16",
  "org.scalatest" %% "scalatest" % "3.2.16" % "test"
)

val aoc = taskKey[Unit]("Create folders for Advent of Code.")
val latestDay = taskKey[Int]("Find the latest day for Advent Of Code.")

aoc := {
  val day = f"${latestDay.value + 1}%02d"

  Files.createDirectories(Paths.get(s"src/main/scala/day$day"))
  Files.createDirectories(Paths.get(s"src/test/scala/day$day"))
  Files.createDirectories(Paths.get(s"src/test/resources/day$day"))
  Files.createFile(Paths.get(s"src/test/resources/day$day/input.txt"))
  Files.createFile(Paths.get(s"src/test/resources/day$day/sample.txt"))

  val daySrcFile = Paths.get(s"src/main/scala/day$day/Day$day.scala")
  Files.createFile(daySrcFile)
  val srcWriter = Files.newBufferedWriter(daySrcFile, StandardCharsets.UTF_8)
  try {
    srcWriter.write(
      s"""|package fr.gplassard.adventofcode
          |package day$day
          |
          |object Day$day {
          |  def part1(lines: List[String]): Int = {
          |    -1
          |  }
          |
          |  def part2(lines: List[String]): Int = {
          |    -1
          |  }
          |}
          |""".stripMargin
    )
  } finally {
    srcWriter.close()
  }

  val dayTestFile = Paths.get(s"src/test/scala/day$day/Day${day}Test.scala")
  Files.createFile(dayTestFile)
  val testWriter = Files.newBufferedWriter(dayTestFile, StandardCharsets.UTF_8)
  try {
    testWriter.write(
      s"""|package fr.gplassard.adventofcode
          |package day$day
          |
          |import org.scalatest.*
          |import org.scalatest.matchers.should.*
          |import org.scalatest.wordspec.AnyWordSpec
          |
          |import java.nio.file.{Files, Paths}
          |import scala.jdk.StreamConverters.*
          |
          |class Day${day}Test extends AnyWordSpec with Matchers {
          |
          |  "part1" should {
          |
          |    "work for the sample" in {
          |      val measures = Files.lines(Paths.get("src/test/resources/day$day/sample.txt")).toScala(List)
          |      Day$day.part1(measures) should equal(-1)
          |    }
          |
          |    "work for the input" in {
          |      val measures = Files.lines(Paths.get("src/test/resources/day$day/input.txt")).toScala(List)
          |      Day$day.part1(measures) should equal(-1)
          |    }
          |  }
          |
          |  "part2" should {
          |
          |    "work for the sample" in {
          |      val measures = Files.lines(Paths.get("src/test/resources/day$day/sample.txt")).toScala(List)
          |      Day$day.part2(measures) should equal(-1)
          |    }
          |
          |    "work for the input" in {
          |      val measures = Files.lines(Paths.get("src/test/resources/day$day/input.txt")).toScala(List)
          |      Day$day.part2(measures) should equal(-1)
          |    }
          |  }
          |}
          |""".stripMargin
    )
  } finally {
    testWriter.close()
  }

  println(s"Day $day folders have been created.")
}


latestDay := {
  val DayPattern: Regex = "day(\\d+)".r

  val directory = Paths.get("src/main/scala")
  val allDays = Files.newDirectoryStream(directory).iterator().asScala
    .filter(Files.isDirectory(_))
    .map(path => DayPattern.findFirstMatchIn(path.getFileName.toString))
    .collect { case Some(regexMatch) => regexMatch.group(1).toInt }
    .toList
  if (allDays.isEmpty) 1 else allDays.max
}
