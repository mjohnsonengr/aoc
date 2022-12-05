import java.io.File

class Part1 {
  fun run(input: String): String {
    // Hard-coded assumption of 9 stacks
    val (cratesIn, cmdIn) = input.split("\n\n")
    val crateRows =
        cratesIn
            .lines()
            .map { line ->
              (1..33 step 4)
                .map { if (it < line.length) line[it] else ' ' } }
            .reversed()
            .drop(1)

    val stacks = transpose(crateRows)
    stacks.also(::println)

    cmdIn
        .lines()
        .map { line ->
          val (num, from, to) =
              Regex("""move (\d+) from (\d+) to (\d+)""").find(line)!!.groupValues.drop(1).map {
                it.toInt()
              }
          Command(num, from, to)
        }
        .forEach { (num, from, to) ->
          val stackFrom = stacks[from - 1]
          val stackTo = stacks[to - 1]
          for (i in 0 until num) {
            stackTo.add(stackFrom.removeLast())
          }
        }

    return stacks.filter { it.isNotEmpty() }.map { it.last() }.joinToString("")
  }
}

data class Command(val num: Int, val from: Int, val to: Int)

fun transpose(list: List<List<Char>>): List<MutableList<Char>> {
  val cols = list[0].size
  val rows = list.size
  return Array(cols) { j -> Array(rows) { i -> list[i][j] } }
      .map { it.filter { it.isLetter() }.toMutableList() }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
