import java.io.File
import kotlin.math.abs

class Part2 {
  fun run(input: String): Int {
    val starts = mutableSetOf<Pos>()
    var goal = Pos(-1,-1)
    val grid =
      input.lines().withIndex().map { (r, line) ->
        line.withIndex().map { (c, it) ->
          when(it) {
            'S' -> {
              starts.add(Pos(r, c))
              0
            }
            'E' -> {
              goal = Pos(r, c)
              25
            }
            else -> {
              val h = it.code - 'a'.code
              if (h == 0) {
                starts.add(Pos(r, c))
              }
              h
            }
          }
        }
      }.toGrid()


    return starts.minOf { start ->
      bfs(start, goal, grid)
    }
  }

  data class Path (val last: Pos, val h: Int, val size: Int)

  fun bfs(start: Pos, goal: Pos, grid: Grid): Int {
    // this is definitely not thread safe
    val visited = mutableSetOf<Pos>()
    var possiblePaths = listOf(Path(start, grid[start], 0))
    while(true) {
      if (possiblePaths.isEmpty()) {
        return Int.MAX_VALUE
      }
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
  println(Part2().run(file.readText().trimIndent()))
}
