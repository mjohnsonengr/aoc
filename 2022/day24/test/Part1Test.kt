import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
            """, 18))

    for (sample in samples) {
      assertEquals(sample.result, Part1(sample.input).run1())
    }
  }

  @Test
  fun input() {
    assertEquals(281, Part1(File("input.txt").readText().trimIndent()).run1())
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
