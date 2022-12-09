import java.io.File

class Part1 {
  fun run(input: String): Int {
    val lines = input.lines()
    val w = lines.first().length
    val h = lines.size

    val trees = lines.map { line ->
      line.map { it.digitToInt() }
    }
    val canSee = mutableSetOf<Pair<Int, Int>>()
    // outer edges
    canSee.addAll((0 until w).flatMap { listOf(0 to it, h-1 to it) })
    canSee.addAll((1 until h-1).flatMap { listOf(it to 0, it to w-1) })

    // check from the top
    var highest = trees.first().toMutableList()
    trees.withIndex().toList().subList(1, h-1).forEach { (i, row) ->
      row.withIndex().toList().subList(1, w-1).forEach { (j, tree) ->
        if (tree > highest[j]) {
          highest[j] = tree
          canSee.add(i to j)
        }
      }
    }

    // check from the bottom
    highest = trees.last().toMutableList()
    trees.withIndex().toList().subList(1, h-1).reversed().forEach { (i, col) ->
      col.withIndex().toList().subList(1, w-1).forEach { (j, tree) ->
        if (tree > highest[j]) {
          highest[j] = tree
          canSee.add(i to j)
        }
      }
    }

    val treeSide = transpose(trees)
    // check from the left
    highest = treeSide.first().toMutableList()
    treeSide.withIndex().toList().subList(1, h-1).forEach { (i, col) ->
      col.withIndex().toList().subList(1, w-1).forEach { (j, tree) ->
        if (tree > highest[j]) {
          highest[j] = tree
          canSee.add(j to i)
        }
      }
    }

    // check from the right
    highest = treeSide.last().toMutableList()
    treeSide.withIndex().toList().subList(1, h-1).reversed().forEach { (i, row) ->
      row.withIndex().toList().subList(1, w-1).forEach { (j, tree) ->
        if (tree > highest[j]) {
          highest[j] = tree
          canSee.add(j to i)
        }
      }
    }

    return canSee.size
  }

  private fun transpose(list: List<List<Int>>): List<List<Int>> {
    val cols = list[0].size
    val rows = list.size
    return Array(cols) { j -> Array(rows) { i -> list[i][j] } }
      .map { it.toList() }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
