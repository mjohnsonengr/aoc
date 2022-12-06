import java.io.File

class Part2 {
  fun run(input: String): Int {
    var sx = 0;
    var sy = 0;
    var rx = 0;
    var ry = 0;
    val santa = mutableSetOf<Pair<Int, Int>>(0 to 0);
    val robo = mutableSetOf<Pair<Int, Int>>(0 to 0);
    input.chunked(2).forEach {
      when (it[0]) {
        '^' -> sy+=1
        '>' -> sx+=1
        '<' -> sx-=1
        'v' -> sy-=1
      }
      when (it[1]) {
        '^' -> ry+=1
        '>' -> rx+=1
        '<' -> rx-=1
        'v' -> ry-=1
      }
      santa.add(sx to sy)
      robo.add(rx to ry)
    }
    return (santa + robo).size
  }

}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
