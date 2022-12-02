import java.io.File

class Part2 {
  fun run(input: String): Int {
    val points =mapOf(
      Pair("X", 0),
      Pair("Y", 3),
      Pair("Z", 6)
    )
    val piece = mapOf(
      Pair("A", 1),
      Pair("B", 2),
      Pair("C", 3)
    )
    val states = mapOf(
      Pair("X", -1),
      Pair("Y", 0),
      Pair("Z", 1)
    )
    // A =  rock (score 1)
    // B =  paper (score 2)
    // C =  scissors (score 3)
    // X = lose
    // Y =tie
    // Z = win
    // win = 6 points
    val result = input.lines().map {
      val plays = Regex("""([A-Z])\s([A-Z])""").find(it)?.groupValues!!
      val (_, other, state) = plays
      val piece = getPiece(piece[other]!!, states[state]!!)
      piece + points[state]!!
    }
    return result.sum()
  }

  // 1 = rock; 2 = paper; 3 = scissors
  fun getPiece(play: Int, state: Int):Int {
    val result = play + state
    if (result == 4) {
      return 1
    }
    if (result ==0) {
      return 3
    }
    return result

  }

}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
