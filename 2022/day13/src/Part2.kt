import java.io.File
import java.lang.AssertionError

class Part2 {
  fun run(input: String): Int {
    val a = input.lines()
      .plus("[[2]]")
      .plus("[[6]]")
      .filter { it.isNotEmpty() }
      .map { parse(it) }
      .sorted()
    val div1 = parse("[[2]]")
    val div2 = parse("[[6]]")

    return (a.indexOf(div1)+1)*(a.indexOf(div2)+1)

  }

  fun parse(packet: String): Lst {
    var lstStack = mutableListOf<Lst>(Lst())
    var inside = mutableListOf<Char>()
    packet.drop(1).dropLast(1).forEach {
      when (it) {
        '[' -> {
          // start a new list
          lstStack.last().addAll(inside)
          lstStack.add(Lst())
          inside.clear()
        }
        ']' -> {
          val oldLst = lstStack.removeLast()
          oldLst.addAll(inside)
          lstStack.last().add(oldLst)
          inside.clear()
        }
        else -> inside.add(it)
      }
    }
    lstStack.last().addAll(inside)

    return lstStack[0]
  }

  abstract class Contents : Comparable<Contents> {
    override operator fun compareTo(other: Contents): Int {
      return when (this) {
        is Num -> {
          when (other) {
            is Num -> {
              this.compareTo(other)
            }
            is Lst -> {
              Lst.of(this).compareTo(other)
            }
            else -> throw AssertionError()
          }
        }
        is Lst -> {
          when (other) {
            is Num -> {
              this.compareTo(Lst.of(other))
            }
            is Lst -> {
              this.compareTo(other)
            }
            else -> throw AssertionError()
          }
        }
        else -> throw AssertionError()
      }
    }
  }

  data class Num(val value: Int) : Contents() {
    operator fun compareTo(other: Num): Int {
      return value.compareTo(other.value)
    }
  }
  data class Lst(val value: MutableList<Contents> = mutableListOf()) : Contents() {
    companion object {
      fun of(c: Contents): Lst {
        return Lst(mutableListOf(c))
      }
    }
    fun add(c: Contents): Boolean {
      return value.add(c)
    }

    fun addAll(c: List<Char>): Boolean {
      return value.addAll(c.joinToString("").split(',').mapNotNull { it.toIntOrNull() }.map { Num(it) })
    }

    operator fun compareTo(other: Lst): Int {
      value.zip(other.value).forEach { (left, right) ->
        val compare = left.compareTo(right)
        if (compare != 0) {
          return compare
        }
      }
      return value.size.compareTo(other.value.size)
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
