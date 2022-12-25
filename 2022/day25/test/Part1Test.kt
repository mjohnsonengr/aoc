import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals

class Part1Test {
  @Test
  fun sampleSlns() {
    val samples =
        listOf(
            Sample("""
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
            """, "2=-1=0"))

    for (sample in samples) {
      assertEquals(sample.result, Part1().run(sample.input))
    }
  }

  @Test
  fun testToDec() {
    assertEquals(1, toDec("1"))
    assertEquals(2, toDec("2"))
    assertEquals(3, toDec("1="))
    assertEquals(4, toDec("1-"))
    assertEquals(5, toDec("10"))
    assertEquals(6, toDec("11"))
    assertEquals(7, toDec("12"))
    assertEquals(8, toDec("2="))
    assertEquals(9, toDec("2-"))
    assertEquals(10, toDec("20"))
    assertEquals(15, toDec("1=0"))
    assertEquals(20, toDec("1-0"))
    assertEquals(2022, toDec("1=11-2"))
    assertEquals(12345, toDec("1-0---0"))
    assertEquals(314159265, toDec("1121-1110-1=0"))
  }

  @Test
  fun testToSnafu() {
    assertEquals(toSnafu(1), "1")
    assertEquals(toSnafu(2), "2")
    assertEquals(toSnafu(3), "1=")
    assertEquals(toSnafu(4), "1-")
    assertEquals(toSnafu(5), "10")
    assertEquals(toSnafu(6), "11")
    assertEquals(toSnafu(7), "12")
    assertEquals(toSnafu(8), "2=")
    assertEquals(toSnafu(9), "2-")
    assertEquals(toSnafu(10), "20")
    assertEquals(toSnafu(15), "1=0")
    assertEquals(toSnafu(20), "1-0")
    assertEquals(toSnafu(2022), "1=11-2")
    assertEquals(toSnafu(12345), "1-0---0")
    assertEquals(toSnafu(314159265), "1121-1110-1=0")
  }

  @Test
  fun input() {
    println(Part1().run(File("input.txt").readText().trimIndent()))
  }

  data class Sample(private val rawInput: String, val result: String) {
    val input = rawInput.trimIndent()
  }
}
