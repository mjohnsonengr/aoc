import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
              2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
            """, 4))
    for (sample in samples) {
      assertEquals(sample.result, Part2().run(sample.input))
    }
  }

  @Test
  fun input() {
    assertEquals(843, Part2().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
