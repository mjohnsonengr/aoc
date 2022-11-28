import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("2x3x4", 58),
            Sample("1x1x10", 43),
            Sample(
              """
                2x3x4
                1x1x10
              """, 101
            ))
    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent().lines()
  }
}
