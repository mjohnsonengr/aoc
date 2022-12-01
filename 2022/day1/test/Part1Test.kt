import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
              1000
              2000
              3000

              4000

              5000
              6000

              7000
              8000
              9000

              10000
              """,
                24000))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent().lines()
  }
}
