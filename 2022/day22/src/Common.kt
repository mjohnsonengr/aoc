enum class Tile(val str: String) {
  VOID(" "),
  OPEN("."),
  WALL("#");

  override fun toString() = str
}

typealias Grid = List<List<Tile>>

enum class Dir {
  R,
  L
}

enum class Axis { HORIZ, VERT }
enum class Orientation { DL, UR }
enum class Facing(val inc: Pos, val axis: Axis, val orientation: Orientation, val value: Int, val str: String) {
  R(Pos(0, 1), Axis.HORIZ, Orientation.UR, 0, ">"),
  D(Pos(1, 0), Axis.VERT, Orientation.DL, 1, "v"),
  L(Pos(0, -1), Axis.HORIZ, Orientation.DL, 2, "<"),
  U(Pos(-1, 0), Axis.VERT, Orientation.UR, 3, "^");
  fun next(pos: Pos) = pos + inc

  fun turn(dir: Dir) = when (this) {
    R -> if (dir == Dir.R) D else U
    D -> if (dir == Dir.R) L else R
    L -> if (dir == Dir.R) U else D
    U -> if (dir == Dir.R) R else L
  }
  override fun toString() = str

}

abstract class Step

data class Turn(val dir: Dir) : Step() {
  override fun toString() = dir.toString()
}

data class Walk(val steps: Int) : Step() {
  override fun toString() = steps.toString()
}

data class Pos(val row: Int, val col: Int) {
  companion object {
    fun of(pos: Pair<Int, Int>) = Pos(pos.first, pos.second)
  }
  operator fun plus(inc: Pos) = Pos(row + inc.row, col + inc.col)
}

fun range(list: List<Tile>) =
  list.indexOfFirst { it != Tile.VOID }..list.indexOfLast { it != Tile.VOID }
val IntRange.size: Int
  get() = this.last - this.first + 1

fun parseGrid(gridIn: String) = gridIn.lines().map { line ->
  line.map {
    when (it) {
      ' ' -> Tile.VOID
      '.' -> Tile.OPEN
      '#' -> Tile.WALL
      else -> throw AssertionError()
    }
  }
}

fun parsePath(pathIn: String): Sequence<Step> {
  val walks =
    Regex("""\d+""").findAll(pathIn).map { Walk(it.value.toInt()) }
  val turns = Regex("""[LR]""").findAll(pathIn).map { Turn(Dir.valueOf(it.value)) }
  // walk has one extra
  val path = walks.zip(turns) { walk, turn -> listOf(walk, turn) }.flatten() + walks.last()
  return path
}

fun debugPrint(width: Int, height: Int, grid: Grid, debugVisited: HashMap<Pos, Facing>) {
  (0 until height).forEach { row ->
    (0 until width).forEach { col ->
      val p = Pos(row, col)
      print(debugVisited[p] ?: grid.nullableGet(p) ?: " ")
    }
    println()
  }
  println()
}

fun List<Tile>.safeGet(col: Int): Tile {
  return if (col >= size) Tile.VOID else get(col)
}

operator fun Grid.get(x: Pos) = this[x.row][x.col]
fun Grid.nullableGet(x: Pos): Tile? {
  if (x.row < 0 || x.col < 0 || x.row > this.size) {
    return null
  }
  val row = this[x.row]
  if (x.col >= row.size) {
    return null
  }
  return row[x.col]
}