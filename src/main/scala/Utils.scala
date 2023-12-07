package aoc2023

object Utils:
    def readInput(filePath: String): List[String] =
        io
            .Source
            .fromResource(filePath)
            .getLines
            .toList
