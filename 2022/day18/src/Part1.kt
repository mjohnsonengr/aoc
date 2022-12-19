import java.io.File
import java.util.HashMap

class Part1 {
  data class Point(val x: Int, val y: Int, val z: Int)

  fun run(input: String): Int {
    val grid = hashMapOf<Point, Boolean>()
    input.lines().forEach { line ->
      val (x, y, z) = line.split(',').map { it.toInt() }
      grid[x, y, z] = true
    }

    val sides = grid.entries.sumOf { cube ->
      // for some reason using sumOf() here results in a type ambiguity between Int and Long
      Dir.values().map { d ->
        if (grid.neighbor(cube.key, d)) 0 else 1
      }.sum()
    }

    return sides
  }

  enum class Dir { NORTH, SOUTH, WEST, EAST, UP, DOWN }

  private operator fun HashMap<Point, Boolean>.get(x: Int, y: Int, z: Int): Boolean? {
    return this[Point(x, y, z)]
  }

  private operator fun HashMap<Point, Boolean>.set(x: Int, y: Int, z: Int, v: Boolean) {
    this[Point(x, y, z)] = v
  }

  private fun HashMap<Point, Boolean>.neighbor(p: Point, d: Dir): Boolean {
    return neighbor(p.x, p.y, p.z, d)
  }

  private fun HashMap<Point, Boolean>.neighbor(x: Int, y: Int, z: Int, d: Dir): Boolean {
    return when(d) {
      Dir.NORTH -> this[Point(x, y+1, z)]
      Dir.SOUTH -> this[Point(x, y-1, z)]
      Dir.EAST -> this[Point(x+1, y, z)]
      Dir.WEST -> this[Point(x-1, y, z)]
      Dir.UP -> this[Point(x, y, z+1)]
      Dir.DOWN -> this[Point(x, y, z-1)]
    } ?: false
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
