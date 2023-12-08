package aoc2023.day2
import aoc2023.Utils

import scala.util.matching.Regex

val gameValues = (12, 13, 14)
val redRegex   = """\d+\s*red""".r
val greenRegex = """\d+\s*green""".r
val blueRegex  = """\d+\s*blue""".r

def part1 =
    val input                   = Utils.readInput("day2.txt")
    def ballInfo2Int(s: String) = s.split(" ")(0).toInt

    input
        .zipWithIndex
        .map: (possibleGame, gameNumber) =>
            (
              possibleGame
                  .split(":")(1)
                  .split(";")
                  .toList
                  .forall: hint =>
                      val red   = redRegex.findFirstIn(hint).map(ballInfo2Int).getOrElse(Int.MinValue)
                      val green = greenRegex.findFirstIn(hint).map(ballInfo2Int).getOrElse(Int.MinValue)
                      val blue  = blueRegex.findFirstIn(hint).map(ballInfo2Int).getOrElse(Int.MinValue)

                      red <= gameValues._1 && green <= gameValues._2 && blue <= gameValues._3
              ,
              gameNumber
            )
        .foldLeft(0): (acc, game) =>
            if game._1 then acc + game._2 + 1 else acc

def part2 =
    val input                   = Utils.readInput("day2.txt")
    def ballInfo2Int(s: String) = s.split(" ")(0).toInt
    def maxColourValue(s: Array[String], regex: Regex) =
        s
            .toList
            .map: hint =>
                regex.findFirstIn(hint).map(ballInfo2Int).getOrElse(Int.MinValue)
            .max
    input
        .map: possibleGame =>
            val maxMap = Map("red" -> 0, "green" -> 0, "blue" -> 0)
            val gameSet = possibleGame
                .split(":")(1)
                .split(";")
            val maxRedValue   = maxColourValue(gameSet, redRegex)
            val maxGreenValue = maxColourValue(gameSet, greenRegex)
            val maxBlueValue  = maxColourValue(gameSet, blueRegex)

            maxRedValue * maxGreenValue * maxBlueValue
        .sum

@main def run: Unit =
    println(s"part1: $part1")
    println(s"part2: $part2")
