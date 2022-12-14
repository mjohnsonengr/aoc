import java.io.File
import kotlin.math.max
import kotlin.math.min

class Part1 {
  fun run(input: String): Int {
    val rockPaths = parse(input)
    val grid = Grid.withRockPaths(rockPaths)
    println(grid)
    var a = 0
    while (dropSand(grid)) {
      a++
    }

    return a
  }

  fun parse(input: String): List<RockPath> {
    return input.lines().map { line -> RockPath(line.split(" -> ").map { Point.of(it) }) }
  }

  fun dropSand(grid: Grid): Boolean {
    var cur = Point(500, 0)
    var nextPos = listOf(cur.down(), cur.downLeft(), cur.downRight())
    // associateBy keeps the last value with same key, so reversed nextPos
    var nextStates = nextPos.reversed().associateBy { grid[it] }
    while (nextStates.keys.contains(State.AIR)) {
      cur = nextStates[State.AIR]!!
      if (cur.y > grid.maxY) {
        return false
      }
      nextPos = listOf(cur.down(), cur.downLeft(), cur.downRight())
      // associateBy keeps the last value with same key, so reversed nextPos
      nextStates = nextPos.reversed().associateBy { grid[it] }
    }
    grid[cur] = State.SAND
    return true
  }

  data class RockPath(val path: List<Point>)
  data class Point(val x: Int, val y: Int) {
    fun down() = Point(x, y + 1)
    fun downLeft() = Point(x - 1, y + 1)
    fun downRight() = Point(x + 1, y + 1)
    companion object {
      fun of(point: String): Point {
        // "xxx,yyy" format
        val (x, y) = point.split(",").map { it.toInt() }
        return Point(x, y)
      }
    }
  }

  data class Grid(val grid: Array<Array<State>>, val offsetX: Int) {
    // positive y is down
    val maxY = grid[0].size
    operator fun get(point: Point): State {
      if (point.x > offsetX && point.y > 0 && point.x < offsetX + grid.size && point.y < maxY) {
        return grid[point.x - offsetX][point.y]
      }
      else {
        return State.AIR
      }
    }
    operator fun set(point: Point, state: State) {
      grid[point.x - offsetX][point.y] = state
    }

    override fun toString(): String {
      return buildString {
        (0 until grid[0].size).forEach { y ->
          (offsetX until offsetX + grid.size).forEach { x -> append(grid[x - offsetX][y]) }
          appendLine()
        }
      }
    }

    companion object {
      fun withRockPaths(rockPaths: List<RockPath>): Grid {
        val allPoints = rockPaths.flatMap { it.path }
        val minX = allPoints.minOf { it.x } - 1
        val maxX = allPoints.maxOf { it.x } + 1
        val minY = 0
        val maxY = allPoints.maxOf { it.y } + 1
        val grid = Array(maxX - minX + 1) { Array(maxY - minY + 1) { State.AIR } }

        rockPaths.forEach { rockPath ->
          rockPath.path.windowed(2).forEach { points ->
            rangeOf(points[0].x, points[1].x).forEach { x ->
              rangeOf(points[0].y, points[1].y).forEach { y -> grid[x - minX][y] = State.ROCK }
            }
          }
        }

        return Grid(grid, minX)
      }
    }
  }
  enum class State(private val char: Char) {
    AIR('.'),
    ROCK('#'),
    SAND('o');

    override fun toString(): String {
      return char.toString()
    }
  }

  companion object {
    fun rangeOf(a: Int, b: Int): IntRange {
      return (min(a, b)..max(a, b))
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
