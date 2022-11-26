package mj3ngr.adventofcode.y2018.day1

import java.io.File

class Part2(inputLines: Sequence<String>) {
  private val lines = inputLines.toList()

  private val freqsSeen: HashSet<Int> = HashSet()

  fun process(): Int {
    var freq = 0
    while (true) {
      for (line in lines) {
        freq += processLine(line)
        if (freqsSeen.contains(freq)) {
          return freq
        }
        freqsSeen.add(freq)
      }
    }
  }

  private fun processLine(line: String): Int {
    val sign = line[0]
    val number = line.drop(1).toInt()
    return when (sign) {
      '+' -> number
      '-' -> -number
      else -> throw IllegalArgumentException("invalid line detected in input")
    }
  }
}

fun main(args: Array<String>) {
  val fileName = args[0]
  File(fileName).useLines { println(Part2(it).process()) }
}