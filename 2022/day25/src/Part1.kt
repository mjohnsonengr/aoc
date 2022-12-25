import java.io.File
import kotlin.math.pow

class Part1 {
  fun run(input: String): String {
    // 2, 1, 0
    // "-" = -1
    // "=" = -2
    // 125s place, 25s place, 5s, 1s
    // "20" == 10
    // "2=" == 8
    return toSnafu(input.lines().sumOf { line -> toDec(line) })
  }
}

fun toDec(line: String) =
    line.reversed().withIndex().sumOf { (p, i) ->
      val n =
          when (i) {
            '2' -> 2
            '1' -> 1
            '0' -> 0
            '-' -> -1
            '=' -> -2
            else -> throw AssertionError()
          }
      n * 5.0.pow(p).toLong()
    }

fun toSnafu(i: Long): String {
  var n = i
  var p = 5
  return buildList {
    while (n != 0) {
      val d = ((n + 2) % 5) - 2
      add(snafuDigit(d))
      n -= d
      n /= 5
    }
  }.reversed().joinToString("")
}

fun snafuDigit(i: Long) = when (i) {
   2L -> '2'
   1L -> '1'
   0L -> '0'
   -1L -> '-'
   -2L -> '='
  else -> throw AssertionError("encountered $i")
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
