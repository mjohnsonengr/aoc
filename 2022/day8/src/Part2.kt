import java.io.File

class Part2 {
  fun run(input: String): Int {
    val lines = input.lines()
    val w = lines.first().length
    val h = lines.size

    val trees = lines.map { line -> line.map { it.digitToInt() } }

    return trees.withIndex().map { (r, row) ->
      row.withIndex().map { (c, tree) ->
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
      .flatMap { it }
      .max()
  }

  fun transpose(list: List<List<Int>>): List<List<Int>> {
    val cols = list[0].size
    val rows = list.size
    return Array(cols) { j -> Array(rows) { i -> list[i][j] } }.map { it.toList() }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
