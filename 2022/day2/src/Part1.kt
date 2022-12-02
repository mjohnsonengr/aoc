import java.io.File

class Part1 {
  fun run(input: String): Int {
    val points =mapOf(
      Pair("X", 1),
      Pair("Y", 2),
      Pair("Z", 3)
    )
    val win = mapOf(
      Pair("A", "Y"),
      Pair("B", "Z"),
      Pair("C", "X")
    )
    val tie = mapOf(
      Pair("A", "X"),
      Pair("B", "Y"),
      Pair("C", "Z")
    )
    // A = X = rock (score 1)
    // B = Y = paper (score 2)
    // C = Z = scissors (score 3)
    // win = 6 points
    val result = input.lines().map {
      val plays = Regex("""([A-Z])\s([A-Z])""").find(it)?.groupValues!!
      val (_, other, my) = plays
      val winpts = if (win[other]!! == my) 6 else 0
      val tiepts = if (tie[other]!! == my) 3 else 0
      val playPts = points[my]!!
      winpts + playPts + tiepts
    }
    return result.sum()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
