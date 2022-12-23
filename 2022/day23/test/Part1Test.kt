import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
            """, 110))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  @Test
  fun input() {
    println(Part1().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
