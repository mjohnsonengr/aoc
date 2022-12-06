import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
          Sample("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
          Sample("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
          Sample("nppdvjthqldpwncqszvftbrmjlhg", 23),
          Sample("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
          Sample("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
        )
          for (sample in samples) {
      assertEquals(sample.result, Part2().run(sample.input))
    }
  }

  @Test
  fun input() {
    println(Part2().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
