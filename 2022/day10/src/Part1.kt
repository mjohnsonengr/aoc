import java.io.File
import java.lang.AssertionError

class Part1 {
  fun run(input: String): Int {
    var x = 1 // register
    var cycles = 1
    val signals = mutableListOf<Int>()
    input.lines().map { line ->

      val words = line.split(' ')
      val cmd = words[0]
      when (cmd) {
        "addx" -> {
          if ((cycles+21) % 40 == 0) {
            println("top ${cycles+1} * $x = ${(cycles+1)*x}")
            signals.add((cycles+1)*x)
          }
          val num = words[1].toInt()
          x += num
          cycles += 2
        }
        "noop" -> {
          cycles += 1
        }
        else -> throw AssertionError("shouldn't happen")
      }
      if ((cycles+20) % 40 == 0) {
        println("bot ${cycles} * $x = ${cycles*x}")
        signals.add(cycles*x)
      }
    }
    return signals.sum()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
