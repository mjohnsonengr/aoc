import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
            """, 33))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  @Test
  fun testSpeed() {
    assertEquals(9, Part1().run("""
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
    """.trimIndent()))
  }

  @Test
  fun input() {
    println(Part1().run(File("input.txt").readText().trimIndent()))
  }

  @Test
  fun testSpeed1() {
    // line 1 is the fastest with 2_138_957 loops on the naive approach.
    // should be a quick one to test improvements
    println(Part1().run(File("input.txt").readLines()[0]))

    // other fast ones in order:
    //listOf(1, 6, 25, 26, 29, 17, 19, 24, 5, 4)
  }

  @Test
  fun testSpeed4() {
    // line 4 is fast and has a non-zero result!
    assertEquals(1, Part1().run(File("input.txt").readLines()[3]))
  }

  @Test
  fun testSpeed15() {
    // line 15 is the worst with 3_643_266_053 loops on the naive approach.
    println(Part1().run(File("input.txt").readLines()[14]))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
