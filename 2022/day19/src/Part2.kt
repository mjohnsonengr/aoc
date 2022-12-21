import java.io.File

class Part2 {
  fun run(input: String): Int {
    return parse(input).take(3).map {
      calculateQualityLevel(32, it)
    }.reduce(Int::times)
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
