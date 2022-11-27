import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
          Sample(")", 1),
          Sample("()())", 5)
        )
    for (sample in samples) {
      assertEquals(sample.result, Part2().run(sample.input))
    }
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent().lines()
  }
}
