import java.io.File

class Part1 {
  fun run(input: String) =
    input.split("\n\n")
      .maxOf { elf ->
        elf.lines()
          .sumOf { it.toInt() } }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
