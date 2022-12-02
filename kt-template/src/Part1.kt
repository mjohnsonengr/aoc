import java.io.File

class Part1 {
  fun run(input: String): Int {
    return input.lines().size
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
