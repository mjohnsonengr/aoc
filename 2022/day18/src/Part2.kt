import java.io.File

class Part2 {
  data class Point(val x: Int, val y: Int, val z: Int) {
    fun neighbor(d: Dir): Point {
      return when (d) {
        Dir.NORTH -> Point(x, y + 1, z)
        Dir.SOUTH -> Point(x, y - 1, z)
        Dir.EAST -> Point(x + 1, y, z)
        Dir.WEST -> Point(x - 1, y, z)
        Dir.UP -> Point(x, y, z + 1)
        Dir.DOWN -> Point(x, y, z - 1)
      }
    }
  }

  data class Bubble private constructor(val index: Int) {
    companion object {
      val atmosphere = Bubble(0)
      private var nextBubble = 1
      fun newBubble() = Bubble(nextBubble++)
    }
  }

  fun run(input: String): Int {
    val lava = hashSetOf<Point>()
    input.lines().forEach { line ->
      val (x, y, z) = line.split(',').map { it.toInt() }
      lava.add(Point(x, y, z))
    }

    val maxX = lava.maxOf { it.x }
    val maxY = lava.maxOf { it.y }
    val maxZ = lava.maxOf { it.z }

    // We could try to make a connected forest of air and see how many distinct forests there are.
    // give each air a "bubble" property; mark each air around the border as the "atmosphere" bubble
    // then for each air:
    //   - get a set of the bubbles attached to each neighbor
    //   - If none found, create a new one
    //   - if multiple found, merge those
    //   - add this point to the resulting bubble
    // Bubble data structure: grid is actually just a numeric reference to a bubble; a bubble is just a mutable
    // set of points in a 2D array. A merge is clearing the row and moving all points to the bubble merged with;
    // merges always retain the bubble with a smaller index
    val atmosphere = Bubble.atmosphere
    val bubbles = mutableMapOf<Bubble, HashSet<Point>>()
    bubbles[atmosphere] = hashSetOf()
    val grid: Array<Array<Array<Bubble?>>> =
      Array(maxX) { _ -> Array(maxY) { _ -> Array(maxZ) { _ -> null } } }

    (0 until maxX).flatMap { x ->
      (0 until maxY).flatMap { y ->
        (0 until maxZ).map { z ->
          Point(x, y, z)
        }
      }
    }
      .filter { !lava.contains(it) }
      .forEach { p ->
        val bubble = Dir.values()
            .mapNotNull { d -> grid[p.neighbor(d)] }
            .distinct()
            .reduceOrNull { a, b ->
              // merge
              val keep = listOf(a, b).minBy { it.index }
              val discard = listOf(a, b).maxBy { it.index }
              val move = bubbles[discard]!!
              move.forEach {
                grid[it] = keep
              }
              bubbles[keep]!!.addAll(move)
              bubbles.remove(discard)
              keep
            } ?: Bubble.newBubble()
        grid[p] = bubble
        bubbles.safeGet(bubble).add(p)
      }

    debugPrint(maxX, maxY, maxZ, grid, bubbles, lava)

    val lavaSa =
      lava.sumOf { cube ->
        // TODO: for some reason using sumOf() here results in a type ambiguity between Int and Long
        Dir.values().map { d -> if (lava.contains(cube.neighbor(d))) 0 else 1 }.sum()
      }

    val airBubbleSa =
      bubbles
        .filter { it.key != atmosphere }
        .values
        .sumOf {
          it.sumOf { p -> Dir.values().count { d -> lava.contains(p.neighbor(d)) } }
        }

    return lavaSa - airBubbleSa
  }

  private fun debugPrint(
    maxX: Int,
    maxY: Int,
    maxZ: Int,
    grid: Array<Array<Array<Bubble?>>>,
    bubbles: MutableMap<Bubble, java.util.HashSet<Point>>,
    lava: HashSet<Point>
  ) {
    println("X = 0")
    (0 until maxX).forEach { x ->
      (0 until maxY).forEach { y ->
        print(y.toString().padStart(2, ' '))
        (0 until maxZ).forEach { z ->
          val p = Point(x, y, z)
          if (lava.contains(p)) {
            print("L")
          }
          else if (grid[p] == Bubble.atmosphere) {
            print(" ")
          } else if (grid[p] == null) {
            print("!")
          } else {
            print("B")
          }
        }
        println()
      }
      print("  ")
      (0 until maxY).forEach { y ->
        print(y % 10)
      }
      println()
      println("*".repeat(maxY))
      println("X = ${x+1}")
      println()
    }
  }

  enum class Dir() {
    NORTH,
    SOUTH,
    WEST,
    EAST,
    UP,
    DOWN
  }

  private fun MutableMap<Bubble, HashSet<Point>>.safeGet(bubble: Bubble): HashSet<Point> {
    return getOrPut(bubble) { hashSetOf() }
  }

  private operator fun Array<Array<Array<Bubble?>>>.get(x: Int, y: Int, z: Int): Bubble? {
    return get(Point(x, y, z))
  }

  private operator fun Array<Array<Array<Bubble?>>>.get(pt: Point): Bubble? {
    if (pt.x < 0 || pt.y < 0 || pt.z < 0
      || pt.x >= size
      || pt.y >= get(0).size
      || pt.z >= get(0)[0].size) {
      return Bubble.atmosphere
    }
    return try {
      this[pt.x][pt.y][pt.z]
    } catch (_: ArrayIndexOutOfBoundsException) {
      null
    }
  }

  private operator fun Array<Array<Array<Bubble?>>>.set(pt: Point, v: Bubble) {
    this[pt.x][pt.y][pt.z] = v
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
