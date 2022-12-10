import java.io.File
import java.lang.AssertionError

class Part2 {
  fun run(input: String): Int {
    var x = 1 // register
    var cycles = 1
    val signals = mutableListOf<Int>()
    printSprite(x, 0)
    input.lines().map { line ->
      val words = line.split(' ')
      val cmd = words[0]
      when (cmd) {
        "addx" -> {
          cycles += 1
          printSprite(x, cycles-1)
          if ((cycles) % 40 == 0) {
            println()
          }
          val num = words[1].toInt()
          x += num
          cycles += 1 // should this be above x+=?
        }
        "noop" -> {
          cycles += 1
        }
        else -> throw AssertionError("shouldn't happen")
      }
      printSprite(x, cycles-1)
      if ((cycles) % 40 == 0) {
        println()
      }
    }
    return 0
  }

  fun printSprite(x: Int, pixel: Int) {
    if ((pixel % 40) in x-1..x+1) {
      print("##")
    } else {
      print("  ")
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
