package aoc2023.day4

import aoc2023.Utils

import scala.math.*

case class Card(number: Int, winningNumbers: Vector[Int], scratchedNumbers: Vector[Int])

val numberRegex = """\d+""".r
val input       = Utils.readInput("day4.txt").toVector

val cards =
    input
        .map: card =>
            val cardNumber                         = numberRegex.findFirstIn(card)
            val cardWithoutNumber                  = card.replaceFirst(numberRegex.toString, "read")
            val (winningNumbers, scratchedNumbers) = cardWithoutNumber.span(_ == '|')

            Card(
              cardNumber.get.toInt,
              numberRegex.findAllIn(winningNumbers).map(_.toInt).toVector,
              numberRegex.findAllIn(scratchedNumbers).map(_.toInt).toVector
            )

val cardsAndMatches =
    cards
        .map: card =>
            val allNumbers = card.winningNumbers ++ card.scratchedNumbers

            (
              card.number,
              allNumbers
                  .groupBy(identity)
                  .filter(_._2.size == 2)
            )

def part1 =
    cardsAndMatches
        .map: matchedNumbers =>
            pow(2d, matchedNumbers._2.size - 1)
                .toInt
        .sum

def part2 =
    var extraCards: Map[Int, Int] = cards.map(_.number -> 1).toMap
    def limitCardNumbers(numbers: Range) =
        numbers.filter(_ <= cardsAndMatches.map(_._1).max)

    cardsAndMatches
        .foreach: cm =>
            val toAddCards     = limitCardNumbers(cm._1 + 1 to cm._1 + cm._2.size)
            val cardAppearance = extraCards.getOrElse(cm._1, 0)

            toAddCards.foreach: tac =>
                extraCards += (tac -> (extraCards.getOrElse(tac, 0) + cardAppearance))

    extraCards
        .values
        .sum

@main def run: Unit =
    println(s"part 1: $part1")
    println(s"part 2: $part2")
