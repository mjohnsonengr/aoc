import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
            """, 1514285714288L))

    for (sample in samples) {
      assertEquals(sample.result, Part2().run(sample.input))
    }
  }

  @Test
  fun canWeCountThatHigh() {
    var a = 0L
    repeat(1_000_000) {
      repeat(1_000_000) {
        a++
      }
    }
  }

  @Test
  fun input() {
    println(Part2().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Long) {
    val input = rawInput.trimIndent()
  }
}
