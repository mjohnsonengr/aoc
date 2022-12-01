import java.io.File

class Part1 {
  fun run(input: List<String>): Int {
    val elves = mutableListOf<Int>()
    var elf = 0
    input.forEach {
      if (it.isEmpty()) {
        elves.add(elf)
        elf = 0
      } else {
        elf += it.toInt()
      }
    }
    return elves.max()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readLines()))
}
