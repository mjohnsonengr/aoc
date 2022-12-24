enum class Dir(val rel: Pos, val str: String) {
  N(Pos(0, -1), "^"),
  E(Pos(1, 0), ">"),
  S(Pos(0, 1), "v"),
  W(Pos(-1, 0), "<");

  override fun toString() = this.str
}

data class Pos(val x: Int, val y: Int)

typealias Blizzards = Map<Pos, List<Dir>>

fun parseWalls(lines: List<String>) = lines
  .withIndex()
  .flatMap { (y, line) ->
    line.withIndex().filter { it.value == '#' }.map { (x, _) -> Pos(x - 1, y - 1) }
  }
  .toSet()

fun parseBlizzards(lines: List<String>) = lines
  .withIndex()
  .flatMap { (y, line) ->
    line.withIndex().mapNotNull { (x, c) ->
      when (c) {
        '<' -> Pos(x - 1, y - 1) to listOf(Dir.W)
        '>' -> Pos(x - 1, y - 1) to listOf(Dir.E)
        'v' -> Pos(x - 1, y - 1) to listOf(Dir.S)
        '^' -> Pos(x - 1, y - 1) to listOf(Dir.N)
        '#' -> null
        '.' -> null
        else -> throw AssertionError()
      }
    }
  }
  .toMap()

fun gcd(aa: Int, bb: Int): Int {
  var b = bb
  var a = aa
  while (b > 0) {
    val temp = b
    b = a % b
    a = temp
  }
  return a
}

fun lcm(a: Int, b: Int) = ((a.toLong() * b) / gcd(a, b)).toInt()