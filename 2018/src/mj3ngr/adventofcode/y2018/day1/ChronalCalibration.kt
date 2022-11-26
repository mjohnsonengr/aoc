package mj3ngr.adventofcode.y2018.day1

import java.io.File

class ChronalCalibration(private val lines: Sequence<String>) {
  var value: Int = 0
    private set

  fun process(): ChronalCalibration {
    for (line in lines) {
      processLine(line)
    }
    return this
  }

  private fun processLine(line: String) {
    val sign = line.get(0)
    val number = line.drop(1).toInt()
    when (sign) {
      '+' -> value += number
      '-' -> value -= number
    }
  }
}

fun main(args: Array<String>) {
  val fileName = args[0]
  File(fileName).useLines { println(ChronalCalibration(it).process().value) }
}