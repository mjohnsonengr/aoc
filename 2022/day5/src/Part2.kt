import java.io.File

class Part2 {

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
        val moved = mutableListOf<Char>()
          for (i in 0 until num) {
            moved.add(stackFrom.removeLast())
          }
        stackTo.addAll(moved.reversed())
      }

    return stacks.filter { it.isNotEmpty() }.map { it.last() }.joinToString("")
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
