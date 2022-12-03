import java.io.File

class Part2 {
  fun run(input: String): Int {
    val lowercase = ('a'..'z').toList().withIndex().associate { it.value to it.index + 1 }
    val uppercase = ('A'..'Z').toList().withIndex().associate { it.value to it.index + 27 }
    val priorities = lowercase + uppercase

    return input
        .lines()
        .withIndex()
        .groupBy { it.index / 3 }
        .map { line ->
          val (elf1, elf2, elf3) =
            line.value.map { it.value }
          val common =
              elf1
                  .filter { elf2.contains(it) && elf3.contains(it) }
                  .toList()
                  .distinct()
                  .also { require(it.size == 1) }[0]
          priorities[common]!!
        }
        .sum()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
