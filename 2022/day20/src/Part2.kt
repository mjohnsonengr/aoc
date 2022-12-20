import java.io.File
import java.util.*
import kotlin.math.abs

class Part2 {
  fun run(input: String): Long {
    val key = 811589153L
    val init = input.lines().map { it.toLong() * key }
    val l = LinkedList(init.withIndex().toList())
    repeat(10) {
      init.withIndex().forEach { v ->
        val (_, x) = v
        val i = l.indexOf(v)
        val ix1 = (i + x) // - (if (x < 0) 1 else 0)
        l.remove(v)
        val ix = (ix1 + l.size * (abs(ix1 / l.size) + 1)) % l.size
        l.add(ix.toInt(), v)
        i
      }
    }
    val offset = l.indexOfFirst { it.value == 0L }
    return (l.elementAt((1000 + offset) % init.size).value +
        l.elementAt((2000 + offset) % init.size).value +
        l.elementAt((3000 + offset) % init.size).value)
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
