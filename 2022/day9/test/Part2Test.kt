import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
      listOf(
        Sample("""
              R 4
              U 4
              L 3
              D 1
              R 4
              D 1
              L 5
              R 2
            """, 1),
        Sample("""
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
        """, 36)
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
