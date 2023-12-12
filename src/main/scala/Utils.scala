package aoc2023

object Utils:
    def readInput(fileName: String): List[String] =
        io
            .Source
            .fromResource(fileName)
            .getLines
            .toList
