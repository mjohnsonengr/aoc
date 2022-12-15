import java.io.File
import kotlin.math.abs

class Part1 {
  fun run(input: String, yToCheck: Int): Int {
    val sensors = input.lines().map { line ->
      val (sx, sy, bx, by) = Regex("""Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""").find(line)!!.groupValues.drop(1).map { it.toInt() }
      val beacon = Beacon(bx, by)
      Sensor(sx, sy, beacon)
    }
    val beacons = sensors.map { it.beacon }
    val checkedRow = mutableSetOf<Int>()
    sensors.forEach { sensor ->
      val radius = sensor.distanceToBeacon()
      val distance = abs(yToCheck - sensor.y)
      val widthAtRow = radius - distance
      checkedRow.addAll(sensor.x - widthAtRow .. sensor.x + widthAtRow)
    }
    checkedRow.removeAll(beacons.filter { it.y == yToCheck }.map { it.x }.toSet())


    return checkedRow.size
  }

  data class Point(val x: Int, val y: Int)
  data class Grid(val grid: List<List<Int>>)
  data class Sensor(val x: Int, val y: Int, val beacon: Beacon) {
    fun distanceToBeacon() = abs(beacon.x - x) + abs(beacon.y - y)
  }
  data class Beacon(val x: Int, val y: Int)
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent(), 2000000))
}
