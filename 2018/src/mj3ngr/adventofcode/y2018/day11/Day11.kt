package mj3ngr.adventofcode.y2018.day11

import kotlin.system.measureTimeMillis

fun test(x: Int, y: Int, serial: Int, expected: Int) {
  println("Test. Expected $expected")
  val power = calcPower(x, y, serial)
  println("power: $power")
  println()
}

fun prod(serial: Int) {
  println("Prod")
  println(run(serial))
}

fun run(serial: Int): String {
  var xAtMax = 0
  var yAtMax = 0
  var sizeAtMax = 0
  var maxVal = 0
  val allPower = preCalcPower(serial)
  var prevSizeGrid = preCalcPower(serial) // starts w/ 1x1
  for (size in 1..300) {
    val curSizeGrid = Array(301) { Array(301) { 0 }}
    for (x in 1..300 - size + 1) {
      for (y in 1..300 - size + 1) {
        val power = addLayer(prevSizeGrid[x][y], x, y, size, allPower)
        val power2 = calcSquarePower(x, y, size, allPower)
        if (power != power2) {
          throw IllegalStateException("WTF?!")
        }
        if (power > maxVal) {
          maxVal = power
          xAtMax = x
          yAtMax = y
          sizeAtMax = size
        }
        curSizeGrid[x][y] = power
      }
    }
    prevSizeGrid = curSizeGrid
  }
  return "$xAtMax,$yAtMax,$sizeAtMax"
}

fun addLayer(current: Int, x: Int, y: Int, size: Int, allPower: Array<Array<Int>>): Int {
  var new = current
  for (y1 in y until y+size) {
    new += allPower[x+size-1][y1]
  }
  for (x1 in x until x+size) {
    new += allPower[x1][y+size-1]
  }
  new += allPower[x+size-1][y+size-1]
  return new
}

fun preCalcPower(serial: Int): Array<Array<Int>> {
  val arr = Array(301) { Array(301) { 0 }}
  for (x in 1..300) {
    for (y in 1..300) {
      arr[x][y] = calcPower(x, y, serial)
    }
  }
  return arr
}

fun calcSquarePower(xLeft: Int, yTop: Int, size: Int, power: Array<Array<Int>>): Int {
  var sum = 0
  for (x in xLeft until xLeft+size) {
    for (y in yTop until yTop+size) {
      sum += power[x][y]
    }
  }
  return sum
}

fun calcPower(x: Int, y: Int, serial: Int): Int {
  return (y * (x + 10) + serial) * (x + 10) / 100 % 10 - 5
}

fun main(args: Array<String>) {
  test(3, 5, 8, 4)
  test(122, 79, 57, -5)
  test(217, 196, 39, 0)
  test(101, 153, 71, 4)

  println("Test. Expected 90,269,16")
  println(run(18))
  println()

  println("Test. Expected 232,251,12")
  println(run(42))
  println()

  val time = measureTimeMillis {
    prod(9110)
  }
  println("Expected: 235,268,13")

  // High score: 1238
  println("prod time: $time")
}