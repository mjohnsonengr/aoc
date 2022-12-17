import java.io.File
import java.lang.AssertionError

class Part1 {
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

  fun run(input: String): Int {
    val setRocks = mutableSetOf<Point>()
    var nextRock = 0
    var nextWind = 0
    var height = 0 // 0 is bottom layer; height is one above bottom layer
    repeat(2022) {
      var rock = rocks[nextRock].move(2, height + 3)
      nextRock = (nextRock + 1) % rocks.size

      while (true) {
        val wind = input[nextWind]
        nextWind = (nextWind + 1) % input.length

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
      height = setRocks.maxOf { it.y } + 1
//      debugPrint(height, setRocks)
    }

    return height
  }

  fun rockIsValid(rock: Rock, setRocks: Set<Point>): Boolean {
    if (rock.points.any { it.y < 0 || it.x < 0 || it.x > 6 || setRocks.contains(it) }) {
      return false
    }
    return true
  }

  fun debugPrint(height: Int, setRocks: Set<Point>, rock: Rock = Rock(listOf())) {
    println(height)
    (height+6 downTo 0).forEach { y ->
      (0..6).forEach { x ->
        val p = Point(x, y)
        if (setRocks.contains(p)) {
          print('#')
        } else if (rock.points.contains(p)) {
          print('@')
        } else {
          print('.')
        }
      }
      println()
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
