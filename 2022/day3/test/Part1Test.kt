import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun input() {
    assertEquals(7889, Part1().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
