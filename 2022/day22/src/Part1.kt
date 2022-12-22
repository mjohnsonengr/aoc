import java.io.File

class Part1 {
  fun run(input: String): Int {
    val (gridIn, pathIn) = input.split("\n\n")
    val grid = parseGrid(gridIn)
    val path = parsePath(pathIn)

    val height = grid.size
    val width = grid.maxOf { it.size }
    val hRanges = grid.map { row -> range(row) }
    val vRanges =
        (0 until width).map { c -> range(grid.map { it.safeGet(c) }) }

    fun Grid.straight(cur: Pos, face: Facing): Sequence<Pair<Pos, Tile>> {
      return generateSequence(cur to this[cur]) { (pos, _) ->
        val hRange = hRanges[pos.row]
        val vRange = vRanges[pos.col]
        val next = Pos(
          (pos.row + face.inc.row - vRange.first + vRange.size) % vRange.size + vRange.first,
          (pos.col + face.inc.col - hRange.first + hRange.size) % hRange.size + hRange.first)
        next to this[next]
      }
    }

    var curPos = Pos(0, hRanges[0].first)
    var curFace = Facing.R
    val debugVisited = hashMapOf<Pos, Facing>()
    path.forEach { step ->
      when (step) {
        is Walk -> {
          val steps =
            grid.straight(curPos, curFace).take(step.steps + 1)
          curPos = steps.windowed(2).firstOrNull { it[1].second == Tile.WALL }
            ?.get(0)?.first
            ?: steps.last().first
          debugVisited.putAll(steps.takeWhile { it.first != curPos}.map { it.first to curFace })
        }
        is Turn -> curFace = curFace.turn(step.dir)
      }
      debugVisited[curPos] = curFace
    }
    debugPrint(width, height, grid, debugVisited)

    return 1000*(curPos.row+1) + 4*(curPos.col+1) + curFace.value
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
