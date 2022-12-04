import java.io.File

class Part2 {
  fun run(input: String): Int {
    return input.lines().filter { line ->
      val result = Regex("""(\d+)-(\d+),(\d+)-(\d+)""").find(line)?.groupValues!!
      val (num1, num2, num3, num4) = result.drop(1).map { it.toInt() }
      val range1 = num1..num2
      val range2 = num3..num4
      range1.intersect(range2).isNotEmpty()
          || range2.intersect(range1).isNotEmpty()
    }.size
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
