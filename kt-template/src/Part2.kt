import java.io.File

class Part2 {
  fun run(input: String): Int {
    return input.lines().size
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
