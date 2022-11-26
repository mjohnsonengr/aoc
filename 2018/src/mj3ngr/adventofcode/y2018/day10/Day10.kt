package mj3ngr.adventofcode.y2018.day10

import java.io.File
import kotlin.math.min

data class Point(val x: Int, val y : Int)

data class MovingPoint(val p: Point, val xv: Int, val yv: Int) {
  val x = p.x
  val y = p.y
  fun advance() = MovingPoint(Point(p.x + xv, p.y + yv), xv, yv)
}

typealias MovingPoints = List<MovingPoint>

fun run(filename: String) {
  var points = parseLines(File(filename).readLines())
  var minArea = Long.MAX_VALUE
  var seconds = 0
  while(true) {
    val newPoints = advancePoints(points)
    val minX = (newPoints.map { it.x }.min() ?: Int.MAX_VALUE).toLong()
    val minY = (newPoints.map { it.y }.min() ?: Int.MAX_VALUE).toLong()
    val maxX = (newPoints.map { it.x }.max() ?: Int.MIN_VALUE).toLong()
    val maxY = (newPoints.map { it.y }.max() ?: Int.MIN_VALUE).toLong()


    val area = (maxX - minX) * (maxY - minY)
    if (area > minArea) {
      break
    }

    minArea = min(minArea, area)
    points = newPoints
    seconds++
  }

  printPoints(points)
  println(seconds)
}

fun parseLines(lines: List<String>): MovingPoints {
  val regex = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".toRegex()
  val points = ArrayList<MovingPoint>()
  for (line in lines) {
    val (x, y, xv, yv) = regex.find(line)!!.destructured.toList().map { it.toInt() }
    points.add(MovingPoint(Point(x, y), xv, yv))
  }
  return points.toList()
}

fun advancePoints(movingPoints: MovingPoints): MovingPoints {
  return movingPoints.map { it.advance() }
}

fun printPoints(movingPoints: MovingPoints) {
  val pointSet = movingPoints.map { it.p }.toSet()
  val minX = movingPoints.map { it.x }.min() ?: Int.MAX_VALUE
  val minY = movingPoints.map { it.y }.min() ?: Int.MAX_VALUE
  val maxX = movingPoints.map { it.x }.max() ?: Int.MIN_VALUE
  val maxY = movingPoints.map { it.y }.max() ?: Int.MIN_VALUE
  for (y in minY..maxY) {
    for (x in minX..maxX) {
      // Print twice for easier readability
      print(if (pointSet.contains(Point(x, y))) '#' else '.')
      print(if (pointSet.contains(Point(x, y))) '#' else '.')
    }
    println()
  }
}

fun main(args: Array<String>) {
  println("Test")
  run("data/2018/10test.txt")
  println("Prod")
  run("data/2018/10.txt")
}
