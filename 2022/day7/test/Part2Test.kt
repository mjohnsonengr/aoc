import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part2Test {
  @Test
  fun sampleSlns() {
    val samples =
      listOf(
        Sample("""
              ${'$'} cd /
              ${'$'} ls
              dir a
              14848514 b.txt
              8504156 c.dat
              dir d
              ${'$'} cd a
              ${'$'} ls
              dir e
              29116 f
              2557 g
              62596 h.lst
              ${'$'} cd e
              ${'$'} ls
              584 i
              ${'$'} cd ..
              ${'$'} cd ..
              ${'$'} cd d
              ${'$'} ls
              4060174 j
              8033020 d.log
              5626152 d.ext
              7214296 k
            """,
          24933642))
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
