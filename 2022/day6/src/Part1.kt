import java.io.File

class Part1 {
  fun run(input: String): Int {
    input.windowed(4).forEachIndexed { i, s ->
      if (s.toList().distinct().size == 4)
        return i+4
      }
    return -1
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
