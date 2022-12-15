import java.io.File
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

class Part2 {
  fun run(input: String, maxCoord: Int): Long {
    // x and y in 0..4_000_000
    // tuning freq = x*4_000_000 + y
    val sensors = input.lines().map { line ->
      val (sx, sy, bx, by) = Regex("""Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""").find(line)!!.groupValues.drop(1).map { it.toInt() }
      val beacon = Beacon(bx, by)
      Sensor(sx, sy, beacon)
    }
    val beacons = sensors.map { it.beacon }
    val covered = Array(maxCoord+1) { mutableListOf<IntRange>() }

    // store range for each line?
    sensors.forEach { sensor ->
      val radius = sensor.distanceToBeacon()
      (max(0, sensor.y - radius)..min(maxCoord, sensor.y + radius)).forEach {y ->
        val distance = abs(y - sensor.y)
        val widthAtRow = radius - distance
        val minX = max(0, sensor.x - widthAtRow)
        val maxX = min(maxCoord, sensor.x + widthAtRow)
        covered[y].add(minX..maxX)
      }
    }
    print(covered)
    println(covered[10])

    // then for each line, try to find an empty space
    (0..maxCoord).forEach { y ->
      val ranges = covered[y].sortedBy { it.first }
      var i = 0
      while (i < ranges.size-1) {
        var cur = ranges[i]
        i++
        var next = ranges[i]
        while (i < ranges.size-1 && next.first < cur.last) {
          cur = cur.first..max(cur.last,next.last)
          i++
          next = ranges[i]
        }
        if (cur.last < next.first) {
          return ans(cur.last+1, y)
        }
      }
    }

    return -1
  }

  fun ans(x: Int, y: Int) = x.toLong() * 4_000_000 + y.toLong()

  data class Point(val x: Int, val y: Int)
  data class Grid(val grid: List<List<Int>>)
  data class Sensor(val x: Int, val y: Int, val beacon: Beacon) {
    fun distanceToBeacon() = abs(beacon.x - x) + abs(beacon.y - y)
  }
  data class Beacon(val x: Int, val y: Int)

}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent(), 4000000))
}
