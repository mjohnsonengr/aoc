import java.io.File

class Part1(input: String) {
  val lines = input.lines()
  val blizzards = parseBlizzards(lines)
  val walls = parseWalls(lines)
  val width = lines.first().length - 2
  val height = lines.size - 2
  val whlcm = lcm(width, height)

  fun Pos.plusWrap(other: Pos) =
    Pos((x + other.x + width) % width, (y + other.y + height) % height)

  operator fun Pos.plus(other: Pos) = Pos(x + other.x, y + other.y)

  fun Blizzards.contains(pos: Pos, bhOffset: Int, bvOffset: Int): Boolean {
    // look for one bhOffset to the left that's going right, bhOffset to the right that's going left
    // bvOffset up going down, bf down going up
    if (get(Pos((pos.x - bhOffset + width) % width, pos.y))?.contains(Dir.E) == true) {
      return true
    }
    if (get(Pos((pos.x + bhOffset) % width, pos.y))?.contains(Dir.W) == true) {
      return true
    }
    if (get(Pos(pos.x, (pos.y - bvOffset + height) % height))?.contains(Dir.S) == true) {
      return true
    }
    if (get(Pos(pos.x, (pos.y + bvOffset) % height))?.contains(Dir.N) == true) {
      return true
    }
    return false
  }

  fun nextBlizzards(blizzards: Blizzards) =
    blizzards
      .flatMap { (pos, blizs) -> blizs.map { b -> pos.plusWrap(b.rel) to b } }
      .groupBy({ it.first }, { it.second })
      .toMap()

  fun debugPrint(pos: Pos, bhOffset: Int, bvOffset: Int) {
    println(pos)
    (-1..height).forEach { y ->
      (-1..width).forEach { x ->
        val p = Pos(x, y)
        if (pos == p) {
          print("E")
        } else if (walls.contains(p)) {
          print("#")
        } else if (blizzards.contains(p, bhOffset, bvOffset)) {
          print("B")
        } else {
          print(".")
        }
      }
      println()
    }
    println()
  }

  fun bfs(start: Pos, end: Pos, blizzards: Blizzards, bfsVisited: MutableSet<Pair<Pos, Int>> = mutableSetOf()): Pair<Blizzards, Int> {
    var bOffset = 0
    var count = 0
    var moves = listOf(start)
    var blz = blizzards
    while (!moves.contains(end)) {
      if (moves.isEmpty()) {
        throw AssertionError()
      }

      val newMoves = mutableListOf<Pos>()
      val nextBlz = nextBlizzards(blz)
      val nextBOffset = (bOffset + 1) % whlcm
      moves.forEach { cur ->
        bfsVisited.add(Pair(cur, bOffset))
        val nextPos =
          (listOf(Dir.E, Dir.S, Dir.W, Dir.N).map { it.rel } + Pos(0, 0))
            .map { cur + it }
            .filter { !walls.contains(it) }
            .filter { !nextBlz.keys.contains(it) }
            .filter { !bfsVisited.contains(Pair(it, nextBOffset)) }
            .filter { !newMoves.contains(it)}
            .filter { (it).let { p -> p.x in 0..width && p.y in -1..height } }
        newMoves.addAll(nextPos)
      }
      moves = newMoves
      blz = nextBlz
      bOffset = nextBOffset
      count++
    }
    return blz to count
  }

  fun run1(): Int {
    val startX = lines.first().withIndex().first { it.value == '.' }.index - 1
    val endX = lines.last().withIndex().first { it.value == '.' }.index - 1
    val start = Pos(startX, -1)
    val end = Pos(endX, height)
    return bfs(start, end, blizzards).second
  }

  fun run2(): Int {
    val startX = lines.first().withIndex().first { it.value == '.' }.index - 1
    val endX = lines.last().withIndex().first { it.value == '.' }.index - 1
    val start = Pos(startX, -1)
    val end = Pos(endX, height)
    val (b1, s1) = bfs(start, end, blizzards)
    val (b2, s2) = bfs(end, start, b1)
    val (_, s3) = bfs(start, end, b2)
    return s1 + s2 + s3
  }
}

fun main() {
  val file = File("input.txt")
  val prog = Part1(file.readText().trimIndent())
  println(prog.run1())
  println(prog.run2())
}
