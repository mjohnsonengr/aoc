import Part2.Companion.INPUT_COORDS
import Part2.Companion.SAMPLE_COORDS
import Part2.CubeFace.Companion.INPUT_MAPPING
import Part2.CubeFace.Companion.SAMPLE_MAPPING
import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
            """, 5031))
    for (sample in samples) {
      assertEquals(sample.result, Part2(SAMPLE_MAPPING, SAMPLE_COORDS).run(sample.input))
    }
  }

  @Test
  fun input() {
    println(Part2(INPUT_MAPPING, INPUT_COORDS).run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: Int) {
    val input = rawInput.trimIndent()
  }
}
