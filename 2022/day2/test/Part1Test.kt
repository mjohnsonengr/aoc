import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample(
                """
A Y
B X
C Z              """,
                15))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
