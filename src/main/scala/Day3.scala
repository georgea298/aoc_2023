package aoc2023.day3
import aoc2023.Utils

case class Position(i: Int, j: Int)

val input = Utils
    .readInput("day3.txt")
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(_.toVector)
    .toVector

def isSymbol(c: Char) = !c.isDigit && c != '.'
def existingAdjacentPositions(pos: Position, maxI: Int, maxJ: Int, noOfDigits: Int): Vector[Position] =
    val staticPositions = Vector(
      Position(pos.i, pos.j),
      Position(pos.i - 1, pos.j),
      Position(pos.i + 1, pos.j)
    )
    val otherPositions = (1 to noOfDigits + 1)
        .flatMap: offset =>
            List(
              Position(pos.i - 1, pos.j - offset),
              Position(pos.i, pos.j - offset),
              Position(pos.i + 1, pos.j - offset)
            )
        .toVector

    (staticPositions ++ otherPositions)
        .filter: pos =>
            pos.i >= 0 && pos.i < maxI && pos.j >= 0 && pos.j < maxJ

def part1 =
    var numberMap: Map[Position, Int] = Map.empty

    for i <- input.indices do
        var currentNumberPart = 0
        var noOfDigits        = 0
        for j <- input(i).indices do
            if input(i)(j).isDigit then
                currentNumberPart = currentNumberPart * 10 + input(i)(j) - '0'
                noOfDigits += 1

            if j + 1 >= input(i).length || !input(i)(j).isDigit then
                val adjacentPositions =
                    existingAdjacentPositions(Position(i, j), input.length, input(i).length, noOfDigits)

                val isPart = adjacentPositions
                    .exists: pos =>
                        isSymbol(input(pos.i)(pos.j))

                if isPart && currentNumberPart > 0 then numberMap += (Position(i, j), currentNumberPart)
                currentNumberPart = 0
                noOfDigits = 0

    numberMap
def part2 =
    val partialAnswer = part1
    val positionPartMap = partialAnswer
        .map: pos =>
            (
              existingAdjacentPositions(
                pos._1,
                input.length,
                input(0).length,
                pos._2.toString.length
              ),
              pos._2
            )
    val adjacentPositions     = positionPartMap.flatMap(_._1).toVector
    val uniqAdjacentPositions = adjacentPositions.toSet

    uniqAdjacentPositions
        .toList
        .filter: entry =>
            adjacentPositions.count(_ == entry) == 2 && input(entry.i)(entry.j) == '*'
        .map: pos =>
            positionPartMap
                .filter(_._1.contains(pos))
                .values
                .toList
        .map(value => value.head * value.tail.head.toLong)
        .sum

@main def run: Unit =
    println(s"part1: ${part1.values.sum}")
    println(s"part2: $part2")
