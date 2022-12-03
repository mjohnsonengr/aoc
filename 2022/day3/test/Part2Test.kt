import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {

  @Test
  fun input() {
    assertEquals(2825, Part2().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
