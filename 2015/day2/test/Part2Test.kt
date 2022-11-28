import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("2x3x4", 34),
            Sample("1x1x10", 14),
            Sample(
              """
                2x3x4
                1x1x10
              """, 48
            ))

    for (sample in samples) {
      assertEquals(sample.result, Part2().run(sample.input))
    }
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent().lines()
  }
}
