package mj3ngr.adventofcode.y2018.day6

import java.io.File
import kotlin.math.abs
import kotlin.math.max

// for each point on an NxM grid, determine which point is closest.
// filter out the infinite areas: anything which counts an edge coordinate as closest.

data class Input(val coords: ArrayList<Coord>, val maxX: Int, val maxY: Int)
data class CoordId(val id: Int)
data class Coord(val id: CoordId, val point: Point)
data class Point(val x: Int, val y: Int)

fun findLargestArea(input: Input): Int {
  val (coords, maxX, maxY) = input

  // For each point on grid, record closest coordinate. -1 is tie
  val grid: HashMap<Point, CoordId> = HashMap()
  val areas: HashMap<CoordId, Int> = HashMap()
  for (x in 0..maxX) {
    for (y in 0..maxY) {
      val closestCoord = findClosestCoord(Point(x, y), coords)
      grid[Point(x, y)] = closestCoord
      areas[closestCoord] = (areas[closestCoord] ?: 0) + 1
    }
  }

  // Filter out the infinite areas
  val edgeCoords: HashSet<CoordId> = HashSet()
  for (x in 0..maxX) {
    edgeCoords.add(grid[Point(x, 0)]!!)
    edgeCoords.add(grid[Point(x, maxY)]!!)
  }
  for (y in 1 until maxY) {
    edgeCoords.add(grid[Point(0, y)]!!)
    edgeCoords.add(grid[Point(maxX, y)]!!)
  }

  return areas.filter { !edgeCoords.contains(it.key) }.values.sortedDescending().first()
}

private fun parseLines(lines: List<String>): Input {
  val coords: ArrayList<Coord> = ArrayList()
  var maxX = 0
  var maxY = 0
  // Read the points
  for ((i, line) in lines.withIndex()) {
    val (x, y) = """(\d+), (\d+)""".toRegex().find(line)!!.destructured.toList().map { it.toInt() }
    coords.add(Coord(CoordId(i), Point(x, y)))
    maxX = max(x, maxX)
    maxY = max(y, maxY)
  }
  return Input(coords, maxX, maxY)
}

fun findClosestCoord(point: Point, coords: List<Coord>): CoordId {
  var closestCoord: Coord? = null
  var closestDistance = Int.MAX_VALUE
  for (coord in coords) {
    val distance = calcTaxiDistance(point, coord.point)
    if (distance < closestDistance) {
      closestDistance = distance
      closestCoord = coord
    } else if (distance == closestDistance) {
      // We now have a tie.
      closestCoord = null
    }
  }
  return closestCoord?.id ?: CoordId(-1)
}

fun calcTaxiDistance(a: Point, b: Point): Int {
  return abs(a.x - b.x) + abs(a.y - b.y)
}

fun findLargestAreaWithinDistance(input: Input, maxTotalDistance: Int): Int {
  val (coords, maxX, maxY) = input

  var regionSize = 0
  for (x in 0..maxX) {
    for (y in 0..maxY) {
      if (findTotalCoordDistance(Point(x, y), coords) < maxTotalDistance) {
        regionSize++
      }
    }
  }

  return regionSize
}

fun findTotalCoordDistance(point: Point, coords: List<Coord>): Int {
  return coords.map { calcTaxiDistance(point, it.point) }.sum()
}

fun main(args: Array<String>) {
  println("Test===")
  run("data/2018/6test.txt", 32)
  println("Prod===")
  run("data/2018/6.txt", 10000)
}

fun run(filename: String, maxTotalDistance: Int) {
  val input = parseLines(File(filename).readLines())
  println(findLargestArea(input))
  println(findLargestAreaWithinDistance(input, maxTotalDistance))
}

