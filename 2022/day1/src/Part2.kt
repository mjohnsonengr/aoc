import java.io.File

class Part2 {
  fun run(input: String): Int =
    input.split("\n\n")
      .map { it -> it.lines().sumOf { it.toInt() } }
      .sortedDescending()
      .take(3).sum()
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText()))
}
