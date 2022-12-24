import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
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
            """, 54))

    for (sample in samples) {
      assertEquals(sample.result, Part1(sample.input).run2())
    }
  }

  @Test
  fun input() {
    println(Part1(File("input.txt").readText().trimIndent()).run2())
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
