import java.io.File

class Part2 {
  fun run(input: String): Int {
    val lines = input.lines()
    val w = lines.first().length
    val h = lines.size

    val trees = lines.map { line -> line.map { it.digitToInt() } }

    // Outer edge will always be 0
    return trees.withIndex().drop(1).dropLast(1).map { (r, row) ->
      row.withIndex().drop(1).dropLast(1).map { (c, tree) ->
        val seeUp =
            (r - 1 downTo 0)
              .map { trees[it][c] }
              .dropLast(1)
              .takeWhile { it < tree }
              .size + 1
        val seeDown =
            (r + 1 until h)
              .map { trees[it][c] }
              .dropLast(1)
              .takeWhile { it < tree }
              .size + 1
        val seeLeft =
            (c - 1 downTo 0)
              .map { trees[r][it] }
              .dropLast(1)
              .takeWhile { it < tree }
              .size + 1
        val seeRight =
            (c + 1 until w)
              .map { trees[r][it] }
              .dropLast(1)
              .takeWhile { it < tree }
              .size + 1
        seeUp * seeDown * seeLeft * seeRight
      }
    }
      .flatten()
      .max()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
