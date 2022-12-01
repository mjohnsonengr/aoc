import java.io.File

class Part2 {
  fun run(input: List<String>): Int {
    val elves = mutableListOf<Long>()
    var elf = 0L
    input.forEach {
      if (it.isEmpty()) {
        elves.add(elf)
        elf = 0
      } else {
        elf += it.toLong()
      }
    }
    elves.add(elf)
    return elves.sortedDescending().take(3).sum().toInt()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readLines()))
}
