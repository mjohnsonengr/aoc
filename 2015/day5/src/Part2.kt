import java.io.File

class Part2 {
  fun run(input: String): Int {
    val vowels = "aeiou"
    val forbidden = listOf("ab", "cd", "pq", "xy")

    // a) contains a pair of any two letters that appear at least twice without overlapping
    //    e.g. xyxy or aabcdefgaa, but not aaa.
    // b) at least one "sandwich" -- repeat letter w/ exactly one in middle (e.g. xyx)

    return input.lines()
      .filter(::hasSandwich)
      .filter(::hasNonOverlappingPair)
      .size
  }

  fun hasNonOverlappingPair(line: String): Boolean {
    val pairsSeen = mutableSetOf<String>();
    var pairOnDeck: String = ""
    line.windowed(2).forEach {
      if (pairsSeen.contains(it)) {
        return true
      }
      if (pairOnDeck.isNotEmpty()) { pairsSeen.add(pairOnDeck) }
      pairOnDeck = it
    }
    return false
  }

  fun hasSandwich(line: String): Boolean {
    return line.windowed(3).any { it[0] == it[2] }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
