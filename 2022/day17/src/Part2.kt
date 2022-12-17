import java.io.File
import java.lang.AssertionError
import kotlin.math.max

class Part2 {
  // y=0 is the floor; +y is up
  data class Point(val x: Int, val y: Int)
  data class Rock(val points: List<Point>) {
    fun move(x: Int, y: Int): Rock {
      return Rock(points.map { Point(it.x + x, it.y + y) })
    }

    fun moveLeft(): Rock {
      return move(-1, 0)
    }

    fun moveRight(): Rock {
      return move(1, 0)
    }

    fun moveDown(): Rock {
      return move(0, -1)
    }
  }

  // Rocks are defined relative to their bottom left
  val rocks =
      listOf(
          Rock(listOf(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0))),
          Rock(listOf(Point(0, 1), Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 1))),
          Rock(listOf(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2))),
          Rock(listOf(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3))),
          Rock(listOf(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1))))

  fun run(input: String): Long {
    val setRocks = hashSetOf<Point>()
    var nextRock = 0
    var nextWind = 0
    var height = 0 // 0 is bottom layer; height is one above bottom layer
    val cycleData = mutableMapOf((0 to 0) to (0 to 0))
    var cycleSize = 0
    var cycleHeight = 0
    var cycleOffset = 0
    var heightOffset = 0
    var rocksUsed = 0

    // cycle finding loop
    outer@ while (true) {
      val (newHeight, newNextRock, newNextWind) = doRock(nextRock, height, input, nextWind, setRocks)
      height = newHeight
      nextRock = newNextRock
      nextWind = newNextWind
      rocksUsed++

      if ((0..6).map { Point(it, height - 1) }.all { setRocks.contains(it) }) {
        if (cycleData.contains(nextRock to nextWind)) {
          val (prevRocksUsed, prevHeight) = cycleData[nextRock to nextWind]!!
          cycleOffset = prevRocksUsed
          heightOffset = prevHeight
          cycleHeight = height - prevHeight
          cycleSize = rocksUsed - cycleOffset
          break@outer
        }
        cycleData[nextRock to nextWind] = rocksUsed to height
      }
    }

    // this many iteration got us to repeat conditions
    require(cycleSize != 0)

    val cycleRepeats = (1_000_000_000_000L - cycleOffset) / cycleSize
    val finishCycle = (1_000_000_000_000L - cycleOffset) % cycleSize

    var finishHeight = 0
    val finishSetRocks = hashSetOf<Point>()
    repeat(finishCycle.toInt()) {
      val (newHeight, newNextRock, newNextWind) = doRock(nextRock, finishHeight, input, nextWind, finishSetRocks)
      finishHeight = newHeight
      nextRock = newNextRock
      nextWind = newNextWind
    }

    return cycleRepeats * cycleHeight + finishHeight + heightOffset
  }

  private fun doRock(
    nextRock: Int,
    height: Int,
    input: String,
    nextWind: Int,
    setRocks: HashSet<Point>
  ): Triple<Int, Int, Int> {
    var windPtr = nextWind
    var rock = rocks[nextRock].move(2, height + 3)

    while (true) {
      val wind = input[windPtr]
      windPtr = (windPtr + 1) % input.length

      // move by wind
      val moved =
        when (wind) {
          '<' -> rock.moveLeft()
          '>' -> rock.moveRight()
          else -> throw AssertionError()
        }

      if (rockIsValid(moved, setRocks)) {
        rock = moved
      }

      // move down
      val down = rock.moveDown()
      if (!rockIsValid(down, setRocks)) {
        break
      }

      rock = down
    }

    setRocks.addAll(rock.points)
    return Triple(max(height, rock.points.maxOf { it.y } + 1), (nextRock + 1) % rocks.size, windPtr)
  }

  fun rockIsValid(rock: Rock, setRocks: Set<Point>): Boolean {
    if (rock.points.any { it.y < 0 || it.x < 0 || it.x > 6 || setRocks.contains(it) }) {
      return false
    }
    return true
  }

  fun debugPrint(height: Int, setRocks: Set<Point>, rock: Rock = Rock(listOf())) {
    println(height)
    (height + 6 downTo 0).forEach { y ->
      println(lineToString(y, setRocks, rock))
    }
  }

  private fun lineToString(y: Int, setRocks: Set<Point>, rock: Rock = Rock(listOf())): String {
    return buildString {
      (0..6).forEach { x ->
        val p = Point(x, y)
        if (setRocks.contains(p)) {
          append('#')
        } else if (rock.points.contains(p)) {
          append('@')
        } else {
          append('.')
        }
      }
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
