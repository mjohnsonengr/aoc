import java.io.File

class Part1 {
  fun run(input: String): Int {
    val result = input.lines().map {
      val other = it[0].code - 'A'.code
      val my = it[2].code - 'X'.code
      val winPts =
        if (my == other) 3
        else if ((my - 1) % 3 == other) 6
        else 0
      val playPts = my % 3 + 1
      winPts + playPts
    }
    return result.sum()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
