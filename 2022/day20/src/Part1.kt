import java.io.File
import java.util.*
import kotlin.math.abs

class Part1 {
  fun run(input: String): Int {
    val init = input.lines().map { it.toInt() }
    val l = LinkedList(init.withIndex().toList())
    init.withIndex().forEach { v ->
      val (_, x) = v
      val i = l.indexOf(v)
      val ix1 = (i + x)// - (if (x < 0) 1 else 0)
      l.remove(v)
      val ix = (ix1 + l.size*(abs(ix1 / l.size) + 1)) % l.size
      l.add(ix, v)
      i
    }
    val offset = l.indexOfFirst { it.value == 0 }
    return (l.elementAt((1000 + offset) % init.size).value
      + l.elementAt((2000 + offset) % init.size).value
      + l.elementAt((3000 + offset) % init.size).value)
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
