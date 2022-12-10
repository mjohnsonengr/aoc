import java.io.File

class Part1 {
  fun run(input: String): Int {
    val lights = Array(1000) { Array(1000) { false } }
    input.lines().forEach { line ->
      val words = line.split(' ')
      val cmd = when (words[0]) {
        "turn" -> when (words[1]) {
          "on" -> Cmd(Op.ON, coords(words[2]), coords(words[4]))
          "off" -> Cmd(Op.OFF, coords(words[2]), coords(words[4]))
          else -> throw IllegalStateException("this shouldn't happen")
        }
        "toggle" -> Cmd(Op.TOGGLE, coords(words[1]), coords(words[3]))
        else -> throw IllegalStateException("this shouldn't happen")
      }
      cmd.run(lights)
    }

    return lights.sumOf { row -> row.count { it } }
  }

  enum class Op(val op: (Boolean) -> Boolean) {
    ON({ _: Boolean -> true}),
    OFF({ _: Boolean -> false}),
    TOGGLE(({ v: Boolean -> !v}));

    fun run(v: Boolean) = op(v)
  }

  data class Cmd(val op: Part1.Op, val coord1: Coord, val coord2: Coord) {
    fun run(lights: Array<Array<Boolean>>) {
      val (x1,y1) = coord1
      val (x2,y2) = coord2
      (x1..x2).forEach { x->
        (y1..y2).forEach { y ->
          lights[x][y] = op.run(lights[x][y])
        }
      }
    }
  }

  data class Coord(val x: Int, val y: Int)
  fun coords(word: String): Coord = word.split(',').map { it.toInt() }.toCoords()
  fun List<Int>.toCoords() = Coord(this[0], this[1])
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
