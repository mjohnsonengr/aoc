import java.io.File
import kotlin.math.abs
import kotlin.math.sign

class Part1 {
  fun run(input: String): Int {
    var headPos = 0 to 0
    var tailPos = 0 to 0
    val tailVisited = mutableSetOf<Pair<Int, Int>>(tailPos)
    input.lines().forEach { line ->
      val words = line.split(' ')
      val dir =
          when (words[0]) {
            "R" -> 1 to 0
            "L" -> -1 to 0
            "D" -> 0 to 1
            "U" -> 0 to -1
            else -> throw IllegalStateException("shouldn't happen")
          }
      val num = words[1].toInt()
//      println("== $line ==")
//      println()
      (1..num).forEach { _ ->
        headPos += dir
        tailPos = tailPos.catchUpTo(headPos)
        tailVisited.add(tailPos)
        // println("$headPos, $tailPos")


//        debugPrint(headPos, tailPos)
      }
    }
    println(tailVisited.minOf { it.first })
    println(tailVisited.maxOf { it.first })
    println(tailVisited.minOf { it.second })
    println(tailVisited.maxOf { it.second })
    return tailVisited.size
  }

  private fun debugPrint(headPos: Pair<Int, Int>, tailPos: Pair<Int, Int>) {
    // (-218..0).forEach { x ->
    //   (-162..36).forEach { y ->
    (-5..1).forEach { y ->
      (-1..5).forEach { x ->
        if (x to y == headPos) {
          print("H")
        } else if (x to y == tailPos) {
          print("T")
        } else if (x to y == 0 to 0) {
          print("s")
        } else {
          print(".")
        }
      }
      println()
    }
    println()
  }

  operator fun Pair<Int, Int>.plus(other: Pair<Int, Int>) =
    first + other.first to second + other.second

  fun Pair<Int, Int>.catchUpTo(other: Pair<Int, Int>): Pair<Int, Int> {
    // assume movements are only ever 1
    val xDif = other.first - first
    val xd = xDif.sign
    val yDif = other.second - second
    val yd = yDif.sign
    if (abs(xDif) > 1 || abs(yDif) > 1) {
      if (xd != 0 && yd != 0) {
        return first + xd to second + yd
      }
      if (xd != 0) {
        return first + xd to second
      }
      if (yd != 0) {
        return first to second + yd
      }
    }
    return first to second
  }
}



fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
