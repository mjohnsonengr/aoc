import java.io.File

class Part1 {
  fun run(input: String): Int {
    var x = 0;
    var y = 0;
    val housesVisited = mutableSetOf<Pair<Int, Int>>(0 to 0);
    input.forEach {
      when (it) {
        '^' -> y+=1
        '>' -> x+=1
        '<' -> x-=1
        'v' -> y-=1
      }
      housesVisited.add(x to y)
    }
    return housesVisited.size
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
