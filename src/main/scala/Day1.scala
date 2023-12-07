package aoc2023.day1
import aoc2023.Utils

def part1 =
    val input: List[String] = Utils.readInput("day1.txt")
    input
        .foldLeft(0): (acc, entry) =>
            acc + (for first <- entry.find(_.isDigit); last <- entry.findLast(_.isDigit)
            yield first * 10 + last).getOrElse(0)

def part2 =
    val digits = Map(
      "one"   -> 1,
      "two"   -> 2,
      "three" -> 3,
      "four"  -> 4,
      "five"  -> 5,
      "six"   -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine"  -> 9
    )

    def mkRegex(digits: List[String]) = s"""\\d|${digits.mkString("|")}""".r

    def toDigit(s: String) =
        if s.head.isDigit then s.head.toInt - '0' else digits.getOrElse(s, digits.getOrElse(s.reverse, 0))

    val input: List[String] = Utils.readInput("day1.txt")
    val regex               = mkRegex(digits.keys.toList)
    val reverseRegex        = mkRegex(digits.map((key: String, value: Int) => (key.reverse, value)).keys.toList)

    input
        .foldLeft(0): (acc, entry) =>
            acc + (for
                first <- regex.findFirstIn(entry)
                last  <- reverseRegex.findFirstIn(entry.reverse)
            yield toDigit(first) * 10 + toDigit(last)).getOrElse(0)

@main def run: Unit =
    println(s"part 1 result: $part1")
    println(s"part 2 result: $part2")
