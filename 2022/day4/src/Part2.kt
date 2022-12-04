import java.io.File

class Part2 {
  fun run(input: String): Int {
    return input.lines().filter { line ->
      isContained(line)
    }.size

  }

  fun isContained(line: String): Boolean {
    val result = Regex("""(\d+)-(\d+),(\d+)-(\d+)""").find(line)?.groupValues!!
    val (num1, num2, num3, num4) = result.drop(1).map { it.toInt() }
    val range1 = num1..num2
    val range2 = num3..num4
    if (range1.toSet().intersect(range2.toSet()).size > 0) {
      return true
    }
    if (range2.toSet().intersect(range1).size > 0) {
      return true
    }
    return false
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
