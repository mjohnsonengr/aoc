import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
            """, 58))

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
