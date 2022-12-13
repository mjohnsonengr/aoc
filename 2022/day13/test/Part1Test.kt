import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
            """, 13))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  @Test
  fun input() {
    println(Part1().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
