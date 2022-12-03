import java.io.File

class Part1 {
  fun run(input: String): Int {
    val lowercase = ('a'..'z').toList()
      .withIndex()
      .map { it.value to it.index + 1 }
      .toMap()
    val uppercase = ('A'..'Z').toList()
      .withIndex()
      .map { it.value to it.index + 27 }
      .toMap()
    val priorities = lowercase + uppercase

    return input.lines()
      .map { line ->
        val line1 = line.substring(0, line.length / 2)
        val line2 = line.substring(line.length / 2)
        val common = line1.filter {
          line2.contains(it)
        }
          .toList()
          .distinct()[0]
        priorities[common]!!
    }.sum()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
