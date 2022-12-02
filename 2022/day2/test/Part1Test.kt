import java.io.File
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
C Z
              """,
                15))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  @Test
  fun wrap() {
    assertEquals(7, Part1().run("C X"))
  }

  @Test
  fun input() {
    assertEquals(13221, Part1().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
