import java.io.File
import kotlin.math.abs

class Part1 {
  fun run(input: String): Int {
    var start = Pos(-1,-1)
    var goal = Pos(-1,-1)
    val grid =
      input.lines().withIndex().map { (r, line) ->
        line.withIndex().map { (c, it) ->
          when(it) {
            'S' -> {
              start = Pos(r, c)
              0
            }
            'E' -> {
              goal = Pos(r, c)
              26
            }
            else -> it.code - 'a'.code
          }
        }
      }.toGrid()


    val a = bfs(start, goal, grid)
    println(traversals)
    return a
  }

  var traversals = 0
  fun traverse(pos: Pos, goal: Pos, grid: Grid, visited: Set<Pos> = setOf()): Int? {
    traversals += 1
    if (pos == goal) {
      return 0
    }
    return Dir.values()
      .map { pos + it }
      .filter { it ->
        !visited.contains(it) && it.isValidIn(grid) && abs(grid[it] - grid[pos]) <= 1
      }.mapNotNull {
        traverse(it, goal, grid, visited + pos)?.let { it + 1 }
      }
      .minOrNull()
  }

  data class Path (val last: Pos, val h: Int, val size: Int)

  fun bfs(start: Pos, goal: Pos, grid: Grid): Int {
    // this is definitely not thread safe
    val visited = mutableSetOf<Pos>()
    var possiblePaths = listOf(Path(start, grid[start], 0))
    while(true) {
      require(possiblePaths.isNotEmpty())
      possiblePaths = possiblePaths.flatMap { path ->
        Dir.values()
          .map { path.last + it }
          .filter {
            !visited.contains(it) && it.isValidIn(grid) && grid[path.last] - grid[it] >= -1
          }
          .map { pos ->
            visited.add(pos)
            if (pos == goal) {
              return path.size + 1
            }
            Path(pos, grid[pos], path.size + 1)
          }
      }
    }
  }
  data class Grid(val grid: List<List<Int>>) {
    operator fun get(pos: Pos) = grid[pos.r][pos.c]
    val rowCount = grid.size
    val colCount = grid[0].size
  }
  fun List<List<Int>>.toGrid() = Grid(this)
  data class Pos(val r: Int, val c: Int) {
    operator fun plus(dir: Dir): Pos {
      return Pos(r + dir.r, c + dir.c)
    }
    fun isValidIn(grid: Grid): Boolean = r >= 0 && c >= 0 && r < grid.rowCount && c < grid.colCount
  }
  enum class Dir(val r: Int, val c: Int) {
    UP(-1, 0),
    DOWN(1, 0),
    LEFT(0, -1),
    RIGHT(0, 1);
  }
}



fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
