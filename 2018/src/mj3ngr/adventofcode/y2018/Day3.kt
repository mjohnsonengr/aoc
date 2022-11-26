package mj3ngr.adventofcode.y2018

import java.io.File

data class Claim(val id: Int, val x: Int, val y: Int, val width: Int, val height: Int)

data class Point(val x: Int, val y: Int)

fun parse(lines: List<String>) {
  val fabric: HashMap<Point, HashSet<Int>> = HashMap()
  for (line in lines) {
    val (id, x, y, width, height) = parseLine(line)
    for (i in x until x+width) {
      for (j in y until y+height) {
        val point = Point(i, j)
        if (fabric[point] == null) {
          fabric[point] = HashSet()
        }
        fabric[point]!!.add(id)
      }
    }
  }
  val pointsWithOverlap = fabric.asSequence().map { it.value }.filter { it.size > 1 }
  println(pointsWithOverlap.count())
  val overlappers = pointsWithOverlap.flatMap { it.asSequence() }.toSet()
  println(1.rangeTo(lines.size).first { !overlappers.contains(it) })
}

val regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".toRegex()
fun parseLine(line: String): Claim {
  val (id, x, y, width, height) = regex.find(line)!!.destructured.toList().map { it.toInt() }

  return Claim(id, x, y, width, height)
}

fun main(args: Array<String>) {
  parse(File("data/2018/3.txt").readLines())
}