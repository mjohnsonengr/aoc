import java.io.File

class Part2 {
  fun run(input: String): Int {
    val result = input.lines().map {
      val other = it[0].code - 'A'.code
      val state = it[2].code - 'X'.code - 1
      val winPts = (state + 1) * 3
      val playPts = Math.floorMod(other+state, 3) + 1
      winPts + playPts
    }

    return result.sum()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
