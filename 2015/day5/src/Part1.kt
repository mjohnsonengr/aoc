import java.io.File

class Part1 {
  fun run(input: String): Int {
    val vowels = "aeiou"
    val forbidden = listOf("ab", "cd", "pq", "xy")

    return input.lines()
      .filter { line -> !line.windowed(2).any { forbidden.contains(it) } }
      .filter { line -> line.filter { vowels.contains(it) }.length >= 3 }
      .filter { line -> line.windowed(2).any { it[0] == it[1] } }
      .size
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
