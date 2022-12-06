import java.io.File

class Part2 {
  fun run(input: String): Int {
    input.windowed(14).forEachIndexed { i, s ->
      if (s.toList().distinct().size == 14)
        return i+14
    }
    return -1
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
