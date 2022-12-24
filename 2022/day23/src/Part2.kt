import java.io.File

class Part2 {
  // negative y is up
  data class Elf(val x: Int, val y: Int) {
    operator fun plus(other: Elf) = Elf(x + other.x, y + other.y)
  }
  enum class Place {
    GROUND,
    ELF
  }
  enum class Dir (val rel: Elf) {
    N(Elf(0, -1)),
    NE(Elf(1, -1)),
    E(Elf(1, 0)),
    SE(Elf(1, 1)),
    S(Elf(0, 1)),
    SW(Elf(-1, 1)),
    W(Elf(-1, 0)),
    NW(Elf(-1, -1));
  }

  fun run(input: String): Int {
    val elves =
      input
        .lines()
        .withIndex()
        .flatMap { (y, line) ->
          line.withIndex().filter { it.value == '#' }.map { (x, _) -> Elf(x, y) }
        }
        .toHashSet()

    val turnDirs = directions().iterator()
    var prevElves = hashSetOf<Elf>()
    var turns = 0
    while (prevElves != elves) {
      prevElves = elves.toHashSet()
      turns++

      val turnDir = turnDirs.next()
      // first half: propose new locations
      val proposals = elves
        // don't propose move if none of the 8 surrounding have any elves
        .filter { elf -> Dir.values().any { elves.contains(elf + it.rel) }}
        .mapNotNull { elf ->
          val propDir = directions(turnDir).take(4).firstOrNull { dir ->
            when (dir) {
              Dir.N -> listOf(Dir.N, Dir.NE, Dir.NW).none { elves.contains(elf + it.rel) }
              Dir.S -> listOf(Dir.S, Dir.SE, Dir.SW).none { elves.contains(elf + it.rel) }
              Dir.W -> listOf(Dir.W, Dir.SW, Dir.NW).none { elves.contains(elf + it.rel) }
              Dir.E -> listOf(Dir.E, Dir.SE, Dir.NE).none { elves.contains(elf + it.rel) }
              else -> throw AssertionError()
            }
          }
          propDir?.let { elf to elf + it.rel }
        }

      // second half: each elf only moves if no other elf moved to that location
      proposals.groupBy { it.second }.filter { it.value.size == 1 }.forEach {
        val (old, new) = it.value[0]
        elves.remove(old)
        elves.add(new)
      }
    }
    val minX = elves.minOf { it.x }
    val minY = elves.minOf { it.y }
    val maxX = elves.maxOf { it.x }
    val maxY = elves.maxOf { it.y }
    // answer:
    return turns
  }

  fun <T> Sequence<T>.repeat() = sequence { while (true) yieldAll(this@repeat) }
  fun directions() = listOf(Dir.N, Dir.S, Dir.W, Dir.E).asSequence().repeat()
  fun directions(start: Dir) = directions().dropWhile { it != start }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
